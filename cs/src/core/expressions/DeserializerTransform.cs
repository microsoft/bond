// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;

    public struct DeserializerControls
    {
        int maxPreallocatedContainerElements;
        int maxPreallocatedBlobBytes;

        // Default settings
        public readonly static DeserializerControls Default;

        // Current active settings
        public static DeserializerControls Active;

        static DeserializerControls()
        {
            Default.MaxPreallocatedContainerElements = 64 * 1024;
            Default.MaxPreallocatedBlobBytes = 64 * 1024 * 1024;
            Active = Default;
        }

        public int MaxPreallocatedContainerElements
        {
            get { return maxPreallocatedContainerElements; }
            set
            {
                if (value < 0)
                {
                    throw new ArgumentOutOfRangeException("value", "Value cannot be negative");
                }
                maxPreallocatedContainerElements = value;
            }
        }

        public int MaxPreallocatedBlobBytes
        {
            get { return maxPreallocatedBlobBytes; }
            set
            {
                if (value < 0)
                {
                    throw new ArgumentOutOfRangeException("value", "Value cannot be negative");
                }
                maxPreallocatedBlobBytes = value;
            }
        }
    }
}

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Internal.Reflection;

    internal class DeserializerTransform<R>
    {
        readonly Factory newObject = New;
        readonly Factory newBonded = New;
        readonly Factory newContainer = New;
        readonly bool inlineNested;
        TypeAlias typeAlias;

        readonly Expression<Func<R, int, object>> deferredDeserialize;
        readonly List<Expression<Func<R, object>>> deserializeFuncs = new List<Expression<Func<R, object>>>();
        readonly Dictionary<Type, int> deserializeIndex = new Dictionary<Type, int>();
        readonly Stack<Type> inProgress = new Stack<Type>();
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

        // Immutable collection types are represented/identified as strings
        // to avoid depending on the System.Collections.Immutable assembly
        // or NuGet package
        static readonly HashSet<string> immutableListSetTypeNames = new HashSet<string>
        {
            "System.Collections.Immutable.ImmutableArray`1",
            "System.Collections.Immutable.ImmutableList`1",
            "System.Collections.Immutable.ImmutableHashSet`1",
            "System.Collections.Immutable.ImmutableSortedSet`1",
        };

        static readonly HashSet<string> immutableMapTypeNames = new HashSet<string>
        {
            "System.Collections.Immutable.ImmutableDictionary`2",
            "System.Collections.Immutable.ImmutableSortedDictionary`2",
        };

        static readonly HashSet<string> immutableCollectionTypeNames = new HashSet<string>(immutableListSetTypeNames.Concat(immutableMapTypeNames));

        public DeserializerTransform(
            Expression<Func<R, int, object>> deferredDeserialize,
            Factory factory,
            bool inlineNested = true)
        {
            this.deferredDeserialize = deferredDeserialize;
            this.inlineNested = inlineNested;

            if (factory != null)
            {
                newObject = newContainer = newBonded = (t1, t2, a) =>
                    factory(t1, t2, a) ?? New(t1, t2, a);
            }
        }

        public DeserializerTransform(
            Expression<Func<R, int, object>> deferredDeserialize,
            bool inlineNested = true,
            Expression<Func<Type, Type, object>> createObject = null,
            Expression<Func<Type, Type, int, object>> createContainer = null)
        {
            this.deferredDeserialize = deferredDeserialize;
            this.inlineNested = inlineNested;

            if (createObject != null)
            {
                newObject = (t1, t2, a) =>
                    Expression.Convert(
                        Expression.Invoke(
                            createObject, 
                            Expression.Constant(t1), 
                            Expression.Constant(t2)), 
                        t1);
            }

            if (createContainer != null)
            {
                newContainer = (t1, t2, a) =>
                    Expression.Convert(
                        Expression.Invoke(
                            createContainer,
                            Expression.Constant(t1),
                            Expression.Constant(t2),
                            a[0]),
                        t1);
            }
        }

        public IEnumerable<Expression<Func<R, object>>> Generate(IParser parser, Type type)
        {
            Audit.ArgNotNull(type, "type");

            typeAlias = new TypeAlias(type);
            Deserialize(parser, null, type, type, true);
            return deserializeFuncs;
        }

        Expression Deserialize(IParser parser, Expression var, Type objectType, Type schemaType, bool initialize)
        {
            var inline = inlineNested && inProgress.Count != 0 && !inProgress.Contains(schemaType) && var != null;
            Expression body;

            inProgress.Push(schemaType);

            if (inline)
            {
                body = Struct(parser, var, schemaType, initialize);

                if (parser.ReaderParam != parser.ReaderValue)
                {
                    body = Expression.Block(
                        new[] { parser.ReaderParam },
                        Expression.Assign(parser.ReaderParam, Expression.Convert(parser.ReaderValue, parser.ReaderParam.Type)),
                        body);
                }
            }
            else
            {
                int index;
                if (!deserializeIndex.TryGetValue(schemaType, out index))
                {
                    index = deserializeFuncs.Count;
                    deserializeIndex[schemaType] = index;
                    deserializeFuncs.Add(null);
                    var result = Expression.Variable(objectType, $"{objectType.Name}_result");
                    deserializeFuncs[index] = Expression.Lambda<Func<R, object>>(
                        Expression.Block(
                            new[] { result },
                            Struct(parser, result, schemaType, true),
                            Expression.Convert(result, typeof(object))),
                        parser.ReaderParam);
                }

                if (var == null)
                    body = null;
                else
                    body = Expression.Assign(var,
                        Expression.Convert(
                            Expression.Invoke(
                                deferredDeserialize,
                                parser.ReaderValue,
                                Expression.Constant(index)),
                            objectType));
            }

            inProgress.Pop();
            return body;
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
                            Value: (fieldParser, fieldType) => FieldValue(
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
                (valueParser, valueType, next, count, arraySegment) =>
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
                (valueParser, elementType, next, count, arraySegment) =>
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

                        var cappedCount = Expression.Variable(typeof(int), container + "_count");
                        beforeLoop = ApplyCountCap(
                            count,
                            cappedCount,
                            DeserializerControls.Active.MaxPreallocatedBlobBytes,
                            Expression.Block(
                                Expression.Assign(index, Expression.Constant(0)),
                                Expression.Assign(array, Expression.NewArrayBounds(typeof(byte), cappedCount))));

                        // If parser didn't provide real element count we may need to resize the array
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
                            ParameterExpression cappedCount = Expression.Variable(typeof(int), container + "_count");
                            beforeLoop = ApplyCountCap(
                                count,
                                cappedCount,
                                DeserializerControls.Active.MaxPreallocatedContainerElements,
                                Expression.Assign(container, newContainer(container.Type, schemaType, cappedCount)));
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
                            var cappedCount = Expression.Variable(typeof(int), container + "_count");
                            beforeLoop = ApplyCountCap(
                                count,
                                cappedCount,
                                DeserializerControls.Active.MaxPreallocatedContainerElements,
                                Expression.Assign(container, newContainer(container.Type, schemaType, cappedCount)));
                        }
                        else
                        {
                            var capacity = container.Type.GetDeclaredProperty("Capacity", count.Type);
                            if (capacity != null)
                            {
                                var cappedCount = Expression.Variable(typeof(int), container + "_count");
                                beforeLoop = ApplyCountCap(
                                    count,
                                    cappedCount,
                                    DeserializerControls.Active.MaxPreallocatedContainerElements,
                                    Expression.Assign(Expression.Property(container, capacity), cappedCount));
                            }
                        }

                        // For System.Collections.Immutable lists/sets, use the builders to construct them, since ICollection<T>.Add()
                        // is not supported for them.
                        var containerGenericTypeDef = container.Type.GetGenericTypeDefinition();
                        if (immutableListSetTypeNames.Contains(containerGenericTypeDef.FullName))
                        {
                            var builderType = container.Type.GetTypeInfo().GetDeclaredNestedType("Builder").MakeGenericType(item.Type);
                            var builder = Expression.Variable(builderType, container + "_builder");
                            var builderAdd = builderType.GetMethod(typeof(ICollection<>), "Add", item.Type);

                            addItem = Expression.Block(
                                Value(valueParser, item, elementType, itemSchemaType, initialize: true),
                                Expression.Call(builder, builderAdd, item));

                            var toBuilderMethod = container.Type.FindMethod("ToBuilder");
                            var toImmutableMethod = builderType.FindMethod("ToImmutable");
                            var constructBuilder = Expression.Assign(builder, Expression.Call(container, toBuilderMethod));
                            var reconstructImmutable = Expression.Assign(container, Expression.Call(builder, toImmutableMethod));

                            parameters = new[] { item, builder };
                            beforeLoop = Expression.Block(beforeLoop, constructBuilder);
                            afterLoop = Expression.Block(afterLoop, reconstructImmutable);
                        }
                        else
                        {
                            var add = container.Type.GetMethod(typeof(ICollection<>), "Add", item.Type);

                            addItem = Expression.Block(
                                Value(valueParser, item, elementType, itemSchemaType, initialize: true),
                                Expression.Call(container, add, item));

                            parameters = new[] { item };
                        }
                    }

                    return Expression.Block(
                        parameters,
                        beforeLoop,
                        ControlExpression.While(next, addItem),
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
                        var cappedCount = Expression.Variable(typeof(int), map + "_count");

                        // TODO: should we use non-default Comparer
                        init = ApplyCountCap(
                            count,
                            cappedCount,
                            DeserializerControls.Active.MaxPreallocatedContainerElements,
                            Expression.Assign(map, newContainer(map.Type, schemaType, cappedCount)));
                    }

                    // For System.Collections.Immutable maps, use the builders to construct them, since
                    // the setter IDictionary<K,V>.Item[] is not supported for them.
                    var mapGenericTypeDef = map.Type.GetGenericTypeDefinition();
                    if (immutableMapTypeNames.Contains(mapGenericTypeDef.FullName))
                    {
                        var builderType = map.Type.GetTypeInfo().GetDeclaredNestedType("Builder").MakeGenericType(itemSchemaType.Key, itemSchemaType.Value);
                        var builder = Expression.Variable(builderType, map + "_builder");
                        var builderAdd = builderType.GetDeclaredProperty(typeof(IDictionary<,>), "Item", value.Type);

                        var addItem = Expression.Block(
                            Value(keyParser, key, keyType, itemSchemaType.Key, initialize: true),
                            nextValue,
                            Value(valueParser, value, valueType, itemSchemaType.Value, initialize: true),
                            Expression.Assign(Expression.Property(builder, builderAdd, new Expression[] { key }), value));

                        var toBuilderMethod = map.Type.FindMethod("ToBuilder");
                        var toImmutableMethod = builderType.FindMethod("ToImmutable");
                        var constructBuilder = Expression.Assign(builder, Expression.Call(map, toBuilderMethod));
                        var reconstructImmutable = Expression.Assign(map, Expression.Call(builder, toImmutableMethod));

                        return Expression.Block(
                            new[] { key, value, builder },
                            init,
                            constructBuilder,
                            ControlExpression.While(nextKey, addItem),
                            reconstructImmutable);
                    }
                    else
                    {
                        var add = map.Type.GetDeclaredProperty(typeof(IDictionary<,>), "Item", value.Type);

                        var addItem = Expression.Block(
                            Value(keyParser, key, keyType, itemSchemaType.Key, initialize: true),
                            nextValue,
                            Value(valueParser, value, valueType, itemSchemaType.Value, initialize: true),
                            Expression.Assign(Expression.Property(map, add, new Expression[] { key }), value));

                        return Expression.Block(
                            new[] { key, value },
                            init,
                            ControlExpression.While(nextKey, addItem));
                    }
                });
        }

        Expression ApplyCountCap(Expression originalCount, ParameterExpression cappedCount, int maxAllowedCount, Expression expression)
        {
            ConstantExpression constantCount = originalCount as ConstantExpression;
            bool isZeroCount = (constantCount != null) && (constantCount.Value.Equals(0));

            return Expression.Block(
                new[] { cappedCount },
                Expression.Assign(cappedCount, originalCount),
                (isZeroCount
                    ? (Expression)Expression.Empty()
                    : Expression.IfThen(
                        Expression.GreaterThan(cappedCount, Expression.Constant(maxAllowedCount)),
                        Expression.Assign(cappedCount, Expression.Constant(maxAllowedCount)))),
                expression);
        }

        Expression FieldValue(IParser parser, Expression var, Expression valueType, Type schemaType, bool initialize)
        {
            Expression body;

            if (schemaType.IsBondStruct() && var.Type.IsValueType())
            {
                // Special handling for properties of struct types: we deserialize into
                // a temp variable and then assign the value to the property.
                var temp = Expression.Variable(var.Type, $"{var.Type.Name}_temp");
                body = Expression.Block(
                    new[] { temp },
                    Value(parser, temp, valueType, schemaType, true),
                    Expression.Assign(var, temp));
            }
            else
            {
                body = Value(parser, var, valueType, schemaType, initialize);
            }

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
                return parser.Bonded(value => Expression.Assign(var, 
                    newBonded(var.Type, schemaType, PrunedExpression.Convert(value, typeof(IBonded)))));
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
            if (type.IsGenericType() && type.GetGenericTypeDefinition() == typeof(IBonded<>))
            {
                var convert = bondedConvert.MakeGenericMethod(type.GetValueType());
                return Expression.Call(arguments[0], convert);
            }
            else if (schemaType.IsBonded())
            {
                schemaType = type;
            }
            else if (schemaType.IsGenericType())
            {
                // All System.Collections.Immutable collections have a static field ImmutableX<T>.Empty
                // which is the simplest constructor.
                var schemaGenericTypeDef = schemaType.GetGenericTypeDefinition();
                if (immutableCollectionTypeNames.Contains(schemaGenericTypeDef.FullName))
                {
                    return Expression.Field(null, schemaType.GetTypeInfo().GetDeclaredField("Empty"));
                }

                schemaType = schemaGenericTypeDef.MakeGenericType(type.GetTypeInfo().GenericTypeArguments);
            }
            else if (schemaType.IsArray)
            {
                var rank = schemaType.GetArrayRank();
                schemaType = rank == 1 ? type.GetElementType().MakeArrayType() : type.GetElementType().MakeArrayType(rank);
            }

            var ctor = schemaType.GetConstructor(arguments.Select(a => a.Type).ToArray());
            return ctor != null ? Expression.New(ctor, arguments) : Expression.New(schemaType);
        }
    }
}
