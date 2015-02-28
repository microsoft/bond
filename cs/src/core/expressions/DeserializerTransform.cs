// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Reflection;
    using System.Threading;

    internal class DeserializerTransform<R>
    {
        public delegate void DeserializeExpressionHandler(Expression<Func<R, object>> expression, Type objectType, int index);
        delegate Expression NewObject(Type type, Type schemaType);
        delegate Expression NewContainer(Type type, Type schemaType, Expression count);
        
        readonly NewObject newObject;
        readonly NewContainer newContainer;
        TypeAlias typeAlias;

        readonly Expression<Func<R, int, object>> deferredDeserialize;
        readonly DeserializeExpressionHandler deserializeExpressionHandler;
        
        readonly IDictionary<Type, TypeState> typeStates;
        readonly object typeStatesSync;
        
        /// <summary>
        /// Keeps track of types on the current stack while traversing the target deserialization type, in order to
        /// detect recursion and avoid inlining.
        /// </summary>
        readonly Stack<Type> encounteredTypes = new Stack<Type>();

        /// <summary>
        /// Keeps track of types for which an expression is/was being generated, in order to skip infinite recursion.
        /// </summary>
        private readonly HashSet<Type> handledTypes = new HashSet<Type>();

        readonly bool noInlining;

        static readonly MethodInfo bondedConvert =
            Reflection.GenericMethodInfoOf((IBonded bonded) => bonded.Convert<object>());
        static readonly MethodInfo bondedDeserialize =
            Reflection.GenericMethodInfoOf((IBonded bonded) => bonded.Deserialize<object>());
        static readonly MethodInfo arrayResize =
            Reflection.GenericMethodInfoOf((object[] o) => Array.Resize(ref o, default(int)));
        static readonly ConstructorInfo arraySegmentCtor =
            typeof(ArraySegment<byte>).GetConstructor(typeof(byte[]), typeof(int), typeof(int));
        static readonly MethodInfo bufferBlockCopy =
            Reflection.MethodInfoOf((byte[] a) => Buffer.BlockCopy(a, default(int), a, default(int), default(int)));

        public DeserializerTransform(
            DeserializeExpressionHandler deserializeExpressionHandler,
            Expression<Func<R, int, object>> deferredDeserialize,
            Expression<Func<Type, Type, object>> createObject = null,
            Expression<Func<Type, Type, int, object>> createContainer = null,
            IDictionary<Type, TypeState> externalTypeStates = null,
            object externalTypeStatesSync = null,
            bool noInlining = false)
        {
            this.deferredDeserialize = deferredDeserialize;
            if (createObject != null)
            {
                newObject = (t1, t2) =>
                    Expression.Convert(
                        Expression.Invoke(
                            createObject, 
                            Expression.Constant(t1), 
                            Expression.Constant(t2)), 
                        t1);
            }
            else
            {
                newObject = (t1, t2) => New(t1, t2);
            }

            if (createContainer != null)
            {
                newContainer = (t1, t2, count) =>
                    Expression.Convert(
                        Expression.Invoke(
                            createContainer,
                            Expression.Constant(t1),
                            Expression.Constant(t2),
                            count),
                        t1);
            }
            else 
            {
                newContainer = (t1, t2, count) => New(t1, t2, count);
            }

            if (externalTypeStates == null)
            {
                externalTypeStates = new Dictionary<Type, TypeState>();
                
                // We won't bother locking if the dictionary is private to this DeserializerTransform
                externalTypeStatesSync = null;
            }
            typeStates = externalTypeStates;
            typeStatesSync = externalTypeStatesSync;

            this.deserializeExpressionHandler = deserializeExpressionHandler;
            this.noInlining = noInlining;
        }

        public void Generate(IParser parser, Type type)
        {
            Audit.ArgNotNull(type, "type");

            typeAlias = new TypeAlias(type);
            Deserialize(parser, null, type, type, true);
        }

        Expression Deserialize(IParser parser, Expression var, Type objectType, Type schemaType, bool initialize)
        {
            var inline = !noInlining && encounteredTypes.Count != 0 && !encounteredTypes.Contains(schemaType) && var != null;
            Expression body;

            encounteredTypes.Push(schemaType);

            if (inline)
            {
                body = Struct(parser, var, schemaType, initialize);

                if (parser.ReaderParam != parser.ReaderValue)
                {
                    body = Expression.Block(
                        new[] { parser.ReaderParam },
                        Expression.Assign(parser.ReaderParam, parser.ReaderValue),
                        body);
                }
            }
            else
            {
                var typeState = GetTypeState(schemaType);
                if (!typeState.Handled && !handledTypes.Contains(schemaType))
                {
                    handledTypes.Add(schemaType);

                    // This type has not been handled yet, prepare an expression and handle it
                    var result = Expression.Variable(objectType, objectType.Name);
                    var expression = Expression.Lambda<Func<R, object>>(
                        Expression.Block(
                            new[] {result},
                            Struct(parser, result, schemaType, true),
                            result),
                        parser.ReaderParam);

                    deserializeExpressionHandler(expression, objectType, typeState.Index);
                    typeState.Handled = true;
                }

                if (var == null)
                    body = null;
                else
                    body = Expression.Assign(var,
                        Expression.Convert(
                            Expression.Invoke(
                                deferredDeserialize,
                                parser.ReaderValue,
                                Expression.Constant(typeState.Index)),
                            objectType));
            }

            encounteredTypes.Pop();
            return body;
        }

        private TypeState GetTypeState(Type type)
        {
            // Lock only if we have to (if state dictionary is external so might be shared)
            if (typeStatesSync != null) Monitor.Enter(typeStatesSync);
            
            try
            {
                TypeState state;
                if (!typeStates.TryGetValue(type, out state))
                {
                    state = new TypeState { Index = typeStates.Count, Handled = false };
                    typeStates.Add(type, state);
                }

                return state;
            }
            finally
            {
                if (typeStatesSync != null) Monitor.Exit(typeStatesSync);
            }
        }

        Expression Struct(IParser parser, Expression var, Type schemaType, bool initialize)
        {
            var body = new List<Expression>();

            if (initialize)
            {
                body.Add(Expression.Assign(var, newObject(var.Type, schemaType)));
            }

            ITransform transform;

            if (parser.HierarchyDepth > schemaType.GetHierarchyDepth())
            {
                // Parser inheritance hierarchy is deeper than the type we are deserializing.
                // Recurse until hierarchies align.
                transform = new Transform(
                    Base: baseParser => Struct(baseParser, var, schemaType, initialize: false));
            }
            else
            {
                var baseType = schemaType.GetBaseSchemaType();

                transform = new Transform(
                    Fields:
                        from field in schemaType.GetSchemaFields()
                        select new Field(
                            Id: field.Id,
                            Value: (fieldParser, fieldType) => CheckedValue(
                                fieldParser,
                                DataExpression.PropertyOrField(var, field.Name),
                                fieldType,
                                field.GetSchemaType(),
                                field.GetDefaultValue() == null),
                            Omitted: () => field.GetModifier() == Modifier.Required ?
                                ThrowExpression.RequiredFieldMissingException(
                                    field.DeclaringType.Name, Expression.Constant(field.Name)) : 
                                Expression.Empty()),
                    Base: baseParser => baseType != null
                        ? Struct(baseParser, Expression.Convert(var, baseType.GetObjectType()), baseType, initialize: false)
                        : Expression.Empty());
            }

            body.Add(parser.Apply(transform));
            return Expression.Block(body);
        }

        Expression Nullable(IParser parser, Expression var, Type schemaType, bool initialize)
        {
            return parser.Container(schemaType.GetBondDataType(),
                (valueParser, valueType, next, count) =>
                {
                    var body = new List<Expression>();

                    if (initialize)
                        body.Add(Expression.Assign(var, Expression.Default(var.Type)));

                    body.Add(ControlExpression.While(next,
                        Value(valueParser, var, valueType, schemaType, initialize: true)));

                    return Expression.Block(body);
                });
        }

        Expression Container(IParser parser, Expression container, Type schemaType, bool initialize)
        {
            var itemSchemaType = schemaType.GetValueType();

            return parser.Container(itemSchemaType.GetBondDataType(),
                (valueParser, elementType, next, count) =>
                {
                    Expression addItem;
                    ParameterExpression[] parameters;
                    Expression beforeLoop = Expression.Empty();
                    Expression afterLoop = Expression.Empty();

                    if (schemaType.IsBondBlob())
                    {
                        var blob = parser.Blob(count);
                        if (blob != null)
                            return typeAlias.Assign(container, blob);

                        // Parser doesn't provide optimized read for blob so we will have to read byte-by-byte.
                        var index = Expression.Variable(typeof(int), "index");
                        var array = Expression.Variable(typeof(byte[]), "array");

                        beforeLoop = Expression.Block(
                            Expression.Assign(index, Expression.Constant(0)),
                            Expression.Assign(array, Expression.NewArrayBounds(typeof(byte), count)));

                        // If parser didn't provide real item count we may need to resize the array
                        var newSize = Expression.Condition(
                            Expression.GreaterThan(index, Expression.Constant(512)),
                            Expression.Multiply(index, Expression.Constant(2)),
                            Expression.Constant(1024));
                        
                        addItem = Expression.Block(
                            Expression.IfThen(
                                Expression.GreaterThanOrEqual(index, Expression.ArrayLength(array)),
                                Expression.Call(null, arrayResize.MakeGenericMethod(typeof(byte)), array, newSize)),
                                valueParser.Scalar(elementType, BondDataType.BT_INT8, value => Expression.Assign(
                                    Expression.ArrayAccess(array, Expression.PostIncrementAssign(index)),
                                    Expression.Convert(value, typeof(byte)))));

                        afterLoop = typeAlias.Assign(
                            container,
                            Expression.New(arraySegmentCtor, array, Expression.Constant(0), index));

                        parameters = new[] { index, array };
                    }
                    else if (container.Type.IsArray)
                    {
                        var arrayElemType = container.Type.GetValueType();
                        var containerResizeMethod = arrayResize.MakeGenericMethod(arrayElemType);

                        if (initialize)
                        {
                            beforeLoop = 
                                Expression.Assign(container, newContainer(container.Type, schemaType, count));
                        }

                        if (arrayElemType == typeof(byte))
                        {
                            var parseBlob = parser.Blob(count);
                            if (parseBlob != null)
                            {
                                var blob = Expression.Variable(typeof(ArraySegment<byte>), "blob");
                                return Expression.Block(
                                    new[] { blob },
                                    beforeLoop,
                                    Expression.Assign(blob, parseBlob),
                                    Expression.Call(null, bufferBlockCopy, new[]
                                    {
                                        Expression.Property(blob, "Array"),
                                        Expression.Property(blob, "Offset"),
                                        container,
                                        Expression.Constant(0),
                                        count
                                    }));
                            }
                        }

                        var i = Expression.Variable(typeof(int), "i");

                        beforeLoop = Expression.Block(
                            beforeLoop,
                            Expression.Assign(i, Expression.Constant(0)));

                        // Resize the array if we've run out of room
                        var maybeResize =
                            Expression.IfThen(
                                Expression.Equal(i, Expression.ArrayLength(container)),
                                Expression.Call(
                                    containerResizeMethod,
                                    container,
                                    Expression.Multiply(
                                        Expression.Condition(
                                            Expression.LessThan(i, Expression.Constant(32)),
                                            Expression.Constant(32),
                                            i),
                                        Expression.Constant(2))));

                        // Puts a single element into the array.
                        addItem = Expression.Block(
                            maybeResize,
                            Value(
                                valueParser,
                                Expression.ArrayAccess(container, i),
                                elementType,
                                itemSchemaType,
                                initialize: true),
                            Expression.PostIncrementAssign(i));

                        // Expanding the array potentially leaves many blank
                        // entries; this resize will get rid of them.
                        afterLoop = Expression.IfThen(
                            Expression.GreaterThan(Expression.ArrayLength(container), i),
                            Expression.Call(containerResizeMethod, container, i));

                        parameters = new[] { i };
                    }
                    else
                    {
                        var item = Expression.Variable(container.Type.GetValueType(), container + "_item");

                        if (initialize)
                        {
                            beforeLoop = Expression.Assign(container, newContainer(container.Type, schemaType, count));
                        }
                        else
                        {
                            var capacity = container.Type.GetDeclaredProperty("Capacity", count.Type);
                            if (capacity != null)
                            {
                                beforeLoop = Expression.Assign(Expression.Property(container, capacity), count);
                            }
                        }

                        var add = container.Type.GetMethod(typeof(ICollection<>), "Add", item.Type);

                        addItem = Expression.Block(
                            Value(valueParser, item, elementType, itemSchemaType, initialize: true),
                            Expression.Call(container, add, item));

                        parameters = new[] { item };
                    }

                    return Expression.Block(
                        parameters,
                        beforeLoop,
                        ControlExpression.While(next,
                            addItem),
                        afterLoop);
                });
        }

        Expression Map(IParser parser, Expression map, Type schemaType, bool initialize)
        {
            var itemSchemaType = schemaType.GetKeyValueType();

            return parser.Map(itemSchemaType.Key.GetBondDataType(), itemSchemaType.Value.GetBondDataType(),
                (keyParser, valueParser, keyType, valueType, nextKey, nextValue, count) =>
                {
                    Expression init = Expression.Empty();

                    var itemType = map.Type.GetKeyValueType();
                    var key = Expression.Variable(itemType.Key, map + "_key");
                    var value = Expression.Variable(itemType.Value, map + "_value");

                    if (initialize)
                    {
                        // TODO: should we use non-default Comparer
                        init = Expression.Assign(map, newContainer(map.Type, schemaType, count));
                    }

                    var add = map.Type.GetDeclaredProperty(typeof(IDictionary<,>), "Item", value.Type);

                    Expression addItem = Expression.Block(
                        Value(keyParser, key, keyType, itemSchemaType.Key, initialize: true),
                        nextValue,
                        Value(valueParser, value, valueType, itemSchemaType.Value, initialize: true),
                        Expression.Assign(Expression.Property(map, add, new Expression[] { key }), value));

                    return Expression.Block(
                        new [] { key, value },
                        init,
                        ControlExpression.While(nextKey,
                            addItem));
                });
        }

        Expression CheckedValue(IParser parser, Expression var, Expression valueType, Type schemaType, bool initialize)
        {
            var body = Value(parser, var, valueType, schemaType, initialize);

            if (schemaType.IsBondContainer() || schemaType.IsBondStruct() || schemaType.IsBondNullable())
            {
                var expectedType = Expression.Constant(schemaType.GetBondDataType());
                return PrunedExpression.IfThenElse(
                    Expression.Equal(valueType, expectedType),
                    body,
                    ThrowExpression.InvalidTypeException(expectedType, valueType));
            }

            return body;
        }

        Expression Value(IParser parser, Expression var, Expression valueType, Type schemaType, bool initialize)
        {
            if (schemaType.IsBondNullable())
                return Nullable(parser, var, schemaType.GetValueType(), initialize);

            if (schemaType.IsBonded())
            {
                var convert = bondedConvert.MakeGenericMethod(var.Type.GetValueType());
                return parser.Bonded(value => Expression.Assign(var, Expression.Call(value, convert)));
            }

            if (schemaType.IsBondStruct())
            {
                if (parser.IsBonded)
                {
                    var deserialize = bondedDeserialize.MakeGenericMethod(schemaType);
                    return parser.Bonded(value => Expression.Assign(var, Expression.Call(value, deserialize)));
                }
                return Deserialize(parser, var, var.Type, schemaType, initialize);
            }

            if (schemaType.IsBondMap())
                return Map(parser, var, schemaType, initialize);
            
            if (schemaType.IsBondContainer())
                return Container(parser, var, schemaType, initialize);

            return parser.Scalar(valueType, schemaType.GetBondDataType(),
                value => typeAlias.Assign(var, PrunedExpression.Convert(value, schemaType)));
        }

        static Expression New(Type type, Type schemaType, params Expression[] arguments)
        {
            if (schemaType.IsGenericType())
            {
                schemaType = schemaType.GetGenericTypeDefinition().MakeGenericType(type.GetGenericArguments());
            }
            else if (schemaType.IsArray)
            {
                var rank = schemaType.GetArrayRank();
                schemaType = rank == 1 ? type.GetElementType().MakeArrayType() : type.GetElementType().MakeArrayType(rank);
            }

            var ctor = schemaType.GetConstructor(arguments.Select(a => a.Type).ToArray());
            if (ctor != null)
            {
                return Expression.New(ctor, arguments);
            }

            return Expression.New(schemaType);
        }

        public class TypeState
        {
            public int Index { get; set; }
            public bool Handled { get; set; }
        }
    }
}
