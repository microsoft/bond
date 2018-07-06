// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Internal.Reflection;

    /// <summary>
    /// Creates expression of type <see cref="IBonded{T}"/> given a object type and value.
    /// </summary>
    /// <param name="objectType">Type of object to be stored in <see cref="IBonded"/></param>
    /// <param name="value">Expression representing the value to be stored in the bonded instance.</param>
    /// <returns>Expression representing creation of bonded with the specified value.</returns>
    public delegate Expression ObjectBondedFactory(Type objectType, Expression value);

    public class ObjectParser : IParser
    {
        static readonly MethodInfo moveNext = Reflection.MethodInfoOf((IEnumerator e) => e.MoveNext());
        static readonly ConstructorInfo arraySegmentCtor = typeof(ArraySegment<byte>).GetConstructor(typeof(byte[]));
        delegate Expression ContainerItemHandler(Expression value, Expression next, Expression count);
        readonly ParameterExpression objParam;
        readonly TypeAlias typeAlias;
        readonly Expression value;
        readonly Type schemaType;
        readonly Type objectType;
        readonly int hierarchyDepth;
        readonly ObjectBondedFactory bondedFactory;

        public ObjectParser(Type type)
            : this(type, null)
        {}

        public ObjectParser(Type type, ObjectBondedFactory bondedFactory)
        {
            typeAlias = new TypeAlias(type);
            value = objParam = Expression.Parameter(typeof(object), "obj");
            objectType = schemaType = type;
            hierarchyDepth = type.GetHierarchyDepth();
            this.bondedFactory = bondedFactory ?? NewBonded;
        }

        ObjectParser(ObjectParser that, Expression value, Type schemaType)
        {
            typeAlias = that.typeAlias;
            objParam = that.objParam;
            bondedFactory = that.bondedFactory;
            this.value = value;
            this.schemaType = schemaType;
            objectType = value.Type;
            hierarchyDepth = schemaType.GetHierarchyDepth();
        }

        public ParameterExpression ReaderParam { get { return objParam; } }
        public Expression ReaderValue { get { return value; } }
        public int HierarchyDepth { get { return hierarchyDepth; } }
        public bool IsBonded { get { return schemaType.IsBonded(); } }

        public Expression Apply(ITransform transform)
        {
            var structVar = Expression.Variable(objectType, $"{objectType.Name}_obj");
            var body = new List<Expression>
            {
                Expression.Assign(structVar, Expression.Convert(objParam, objectType)),
                transform.Begin
            };

            var baseType = schemaType.GetBaseSchemaType();
            if (baseType != null)
            {
                var baseObject = Expression.Convert(structVar, objectType.GetBaseSchemaType());
                body.Add(transform.Base(new ObjectParser(this, baseObject, baseType)));
            }

            // Performs left outer join of object fields with transform fields.
            // The result contains entry for each schema field. For fields not handled 
            // by the transform default to Skip.
            body.AddRange(
                from objectField in schemaType.GetSchemaFields()
                join transfromField in transform.Fields on objectField.Id equals transfromField.Id into fields
                from knownField in fields.DefaultIfEmpty()
                select Field(transform, structVar, objectField.Id, objectField, knownField));

            body.Add(transform.End);
            
            return Expression.Block(
                new [] { structVar },
                body);
        }

        Expression Field(ITransform transform, Expression structVar, UInt16 id, ISchemaField schemaField, IField field)
        {
            var fieldSchemaType = schemaField.GetSchemaType();
            var fieldId = Expression.Constant(id);
            var fieldType = Expression.Constant(fieldSchemaType.GetBondDataType());
            var fieldValue = DataExpression.PropertyOrField(structVar, schemaField.Name);

            ObjectParser parser = null;

            Expression blob = null;
            ParameterExpression convertedBlob = null;

            // To avoid calling Convert multiple times on the same aliased Blob
            // we must construct a new ObjectParser with the expected return type of
            // of Convert
            if (fieldSchemaType.IsBondBlob())
            {
                blob = typeAlias.Convert(fieldValue, fieldSchemaType);
                convertedBlob = Expression.Variable(blob.Type, "convertedBlob");

                if (blob.Type != fieldValue.Type)
                {
                    parser = new ObjectParser(this, convertedBlob, convertedBlob.Type);
                }
            }

            parser = parser ?? new ObjectParser(this, fieldValue, fieldSchemaType);

            var processField = field != null
                ? field.Value(parser, fieldType)
                : transform.UnknownField(parser, fieldType, fieldId) ?? Expression.Empty();

            var omitField = field != null
                ? field.Omitted : Expression.Empty();

            Expression cannotOmit;

            if (fieldSchemaType.IsBondStruct() || fieldSchemaType.IsBonded() || schemaField.GetModifier() != Modifier.Optional)
            {
                cannotOmit = Expression.Constant(true);

                if (fieldSchemaType.IsBondBlob())
                {
                    return Expression.Block(
                        new[] { convertedBlob },
                        Expression.Assign(convertedBlob, blob),
                        processField);
                }
            }
            else if (fieldSchemaType.IsBondBlob())
            {
                var notEqual = Expression.NotEqual(
                    convertedBlob,
                    Expression.Default(typeof(ArraySegment<byte>)));

                return Expression.Block(
                    new[] { convertedBlob },
                    Expression.Assign(convertedBlob, blob),
                    PrunedExpression.IfThenElse(notEqual, processField, omitField));
            }
            else
            {
                var defaultValue = schemaField.GetDefaultValue();

                if (defaultValue == null)
                {
                    cannotOmit = Expression.NotEqual(fieldValue, Expression.Constant(null));
                }
                else if (fieldSchemaType.IsBondContainer())
                {
                    cannotOmit = Expression.NotEqual(ContainerCount(fieldValue), Expression.Constant(0));
                }
                else
                {
                    var comparand = defaultValue.GetType() != fieldValue.Type
                                        ? (Expression)Expression.Default(fieldValue.Type)
                                        : Expression.Constant(defaultValue);
                    cannotOmit = Expression.NotEqual(fieldValue, comparand);
                }
            }

            return PrunedExpression.IfThenElse(cannotOmit, processField, omitField);
        }

        public Expression Container(BondDataType? expectedType, ContainerHandler handler)
        {
            if (schemaType.IsBondNullable())
                return Nullable(handler);

            if (schemaType.IsBondBlob())
                return BlobContainer(handler);

            var itemType = schemaType.GetValueType();

            ContainerItemHandler itemHandler = (item, next, count) => handler(
                new ObjectParser(this, item, itemType),
                Expression.Constant(itemType.GetBondDataType()),
                next,
                count,
                null);

            if (value.Type.IsArray)
                return ArrayContainer(itemHandler);

            if (value.Type.IsGenericType())
            {
                if (typeof(IList<>).MakeGenericType(value.Type.GetTypeInfo().GenericTypeArguments[0]).IsAssignableFrom(value.Type))
                    return ListContainer(itemHandler);

                if (typeof(LinkedList<>) == value.Type.GetGenericTypeDefinition())
                    return LinkedListContainer(itemHandler);
            }

            return EnumerableContainer(itemHandler);
        }

        public Expression Map(BondDataType? expectedKeyType, BondDataType? expectedValueType, MapHandler handler)
        {
            Debug.Assert(schemaType.IsBondMap());

            var itemType = schemaType.GetKeyValueType();
            return EnumerableContainer((item, next, count) => handler(
                new ObjectParser(this, Expression.Property(item, "Key"), itemType.Key),
                new ObjectParser(this, Expression.Property(item, "Value"), itemType.Value),
                Expression.Constant(itemType.Key.GetBondDataType()),
                Expression.Constant(itemType.Value.GetBondDataType()),
                next,
                Expression.Empty(),
                count));
        }

        public Expression Scalar(Expression valueType, BondDataType expectedType, ValueHandler handler)
        {
            Debug.Assert(expectedType == schemaType.GetBondDataType());
            return handler(typeAlias.Convert(value, schemaType));
        }

        public Expression Bonded(ValueHandler handler)
        {
            if (schemaType.IsBonded())
            {
                return handler(value);
            }

            var newBonded = bondedFactory(objectType, value);
            return handler(newBonded);
        }

        public Expression Blob(Expression count)
        {
            if (schemaType.IsBondBlob())
                return typeAlias.Convert(value, schemaType);

            if (objectType == typeof(byte[]))
                return Expression.New(arraySegmentCtor, value);

            // TODO: convert List<sbyte> to ArraySegment<byte> for faster serialization?
            return null;
        }

        public Expression Skip(Expression valueType)
        {
            return Expression.Empty();
        }

        public override bool Equals(object that)
        {
            Debug.Assert(that is ObjectParser);
            return schemaType.IsBondStruct() && schemaType == (that as ObjectParser).schemaType;
        }

        public override int GetHashCode()
        {
            return schemaType.GetHashCode();
        }

        static Expression NewBonded(Type objectType, Expression value)
        {
            var ctor = typeof(Bonded<>).MakeGenericType(objectType).GetConstructor(objectType);
            return Expression.New(ctor, value);
        }

        static Expression ContainerCount(Expression container)
        {
            if (container.Type.IsArray)
                return Expression.ArrayLength(container);

            if (container.Type.IsBondBlob())
                return Expression.Property(container, "Count");

            return Expression.Property(container, container.Type.GetDeclaredProperty(typeof(ICollection<>), "Count", typeof(int)));
        }

        Expression EnumerableContainer(ContainerItemHandler handler)
        {
            Debug.Assert(schemaType.IsBondContainer());

            var methodGetEnumerator = value.Type.GetMethod(typeof(IEnumerable<>), "GetEnumerator");
            Debug.Assert(methodGetEnumerator != null, "Container must provide GetEnumerator method");

            var enumerator = Expression.Variable(methodGetEnumerator.ReturnType, "enumerator");
            var item = Expression.Property(enumerator, "Current");
            var next = Expression.Call(enumerator, moveNext);

            return Expression.Block(
                new[] { enumerator },
                Expression.Assign(enumerator, Expression.Call(value, methodGetEnumerator)),
                handler(item, next, ContainerCount(value)));
        }

        Expression ListContainer(ContainerItemHandler handler)
        {
            Debug.Assert(schemaType.IsBondContainer());

            var count = Expression.Variable(typeof(int), "count");
            var index = Expression.Variable(typeof(int), "index");
            var item = Expression.Property(value, "Item", new Expression[] { index });
            var next = Expression.LessThan(Expression.PreIncrementAssign(index), count);

            return Expression.Block(
                new[] { index, count },
                Expression.Assign(index, Expression.Constant(-1)),
                Expression.Assign(count, ContainerCount(value)),
                handler(item, next, count));
        }

        Expression ArrayContainer(ContainerItemHandler handler)
        {
            Debug.Assert(schemaType.IsBondContainer());

            var count = Expression.Variable(typeof(int), "count");
            var index = Expression.Variable(typeof(int), "index");
            var item = Expression.ArrayAccess(value, new Expression[] { index });
            var next = Expression.LessThan(Expression.PreIncrementAssign(index), count);

            return Expression.Block(
                new[] { index, count },
                Expression.Assign(index, Expression.Constant(-1)),
                Expression.Assign(count, Expression.ArrayLength(value)),
                handler(item, next, count));
        }

        Expression LinkedListContainer(ContainerItemHandler handler)
        {
            Debug.Assert(schemaType.IsBondContainer());

            var nodeType = typeof(LinkedListNode<>).MakeGenericType(value.Type.GetTypeInfo().GenericTypeArguments[0]);
            var node = Expression.Variable(nodeType, "node");
            var item = Expression.Property(node, "Value");
            var next = Expression.NotEqual(
                Expression.Condition(
                    Expression.Equal(node, Expression.Constant(null)),
                    Expression.Assign(node, Expression.Property(value, "First")),
                    Expression.Assign(node, Expression.Property(node, "Next"))),
                Expression.Constant(null));

            return Expression.Block(
                new[] { node },
                Expression.Assign(node, Expression.Constant(null, nodeType)),
                handler(item, next, ContainerCount(value)));
        }

        Expression Nullable(ContainerHandler handler)
        {
            Debug.Assert(schemaType.IsBondNullable());

            var valueType = schemaType.GetValueType();
            var count = Expression.Variable(typeof(int), "count");

            ParameterExpression convertedBlob = null;
            var nullableValue = value;
            var valueParser = new ObjectParser(this, value, valueType);

            if (valueType.IsBondBlob()) {
                convertedBlob = Expression.Variable(typeof(ArraySegment<byte>), "convertedBlob");
                nullableValue = Expression.Property(convertedBlob, "Array");
                valueParser = new ObjectParser(this, convertedBlob, convertedBlob.Type);
            }

            var notNull = Expression.NotEqual(nullableValue, Expression.Constant(null));

            var loop = handler(
                valueParser,
                Expression.Constant(valueType.GetBondDataType()),
                Expression.NotEqual(Expression.PostDecrementAssign(count), Expression.Constant(0)),
                count,
                null);

            if (convertedBlob != null)
            {
                return Expression.Block(
                    new[] { convertedBlob, count },
                    Expression.Assign(convertedBlob, typeAlias.Convert(value, valueType)),
                    Expression.Assign(count, Expression.Condition(notNull, Expression.Constant(1), Expression.Constant(0))),
                    loop);
            }
            else
            {
                return Expression.Block(
                    new[] { count },
                    Expression.Assign(count, Expression.Condition(notNull, Expression.Constant(1), Expression.Constant(0))),
                    loop);
            }
        }

        Expression BlobContainer(ContainerHandler handler)
        {
            Debug.Assert(schemaType.IsBondBlob());

            var arraySegment = Expression.Variable(typeof(ArraySegment<byte>), "arraySegment");
            var count = Expression.Variable(typeof(int), "count");
            var index = Expression.Variable(typeof(int), "index");
            var end = Expression.Variable(typeof(int), "end");
            var blob = typeAlias.Convert(value, schemaType);
            var item = Expression.ArrayIndex(Expression.Property(arraySegment, "Array"), Expression.PostIncrementAssign(index));

            var loop = handler(
                new ObjectParser(this, item, typeof(sbyte)),
                Expression.Constant(BondDataType.BT_INT8),
                Expression.LessThan(index, end),
                count,
                arraySegment);

            return Expression.Block(
                new[] { arraySegment, count, index, end },
                Expression.Assign(arraySegment, blob),
                Expression.Assign(index, Expression.Property(arraySegment, "Offset")),
                Expression.Assign(count, Expression.Property(arraySegment, "Count")),
                Expression.Assign(end, Expression.Add(index, count)),
                loop);
        }
    }
}
