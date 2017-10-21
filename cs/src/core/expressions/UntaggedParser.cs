// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using System.Linq.Expressions;
    using Bond.Internal.Reflection;

    public class UntaggedParser<R> : IParser
    {
        static readonly ITransform skipStructTransform = new Transform(
            Base: baseParser => baseParser.Skip(Expression.Constant(BondDataType.BT_STRUCT)));
        RuntimeSchema schema;
        readonly UntaggedReader<R> reader;
        readonly DeferredSkip deferredSkip;
        readonly int hierarchyDepth;
        readonly PayloadBondedFactory bondedFactory;

        class DeferredSkip
        {
            public readonly List<Action<R>> Lambdas = new List<Action<R>>();
            public readonly Dictionary<RuntimeSchema, int> Index =
                new Dictionary<RuntimeSchema, int>(new TypeDefComparer());
            public readonly HashSet<RuntimeSchema> InProgress =
                new HashSet<RuntimeSchema>(new TypeDefComparer());
        }

        public UntaggedParser(RuntimeSchema schema)
            : this(schema, null)
        {}

        public UntaggedParser(RuntimeSchema schema, PayloadBondedFactory bondedFactory)
            : this(new UntaggedReader<R>(), new DeferredSkip(), schema, bondedFactory)
        {
            Audit.ArgRule(schema.HasValue, "UntaggedParser requires runtime schema");
        }

        public UntaggedParser(Type type)
            : this(type, null)
        { }

        public UntaggedParser(Type type, PayloadBondedFactory bondedFactory)
            : this(Schema.GetRuntimeSchema(type), bondedFactory)
        {
            Audit.ArgNotNull(type, "type");
        }

        UntaggedParser(UntaggedParser<R> that, RuntimeSchema schema)
            : this(that.reader, that.deferredSkip, schema, that.bondedFactory)
        {}

        UntaggedParser(UntaggedReader<R> reader, DeferredSkip deferredSkip, RuntimeSchema schema, PayloadBondedFactory bondedFactory)
        {
            this.reader = reader;
            this.schema = schema;
            this.deferredSkip = deferredSkip;
            this.bondedFactory = bondedFactory ?? NewBonded;
            hierarchyDepth = schema.GetHierarchyDepth();
        }

        public ParameterExpression ReaderParam { get { return reader.Param; } }
        public Expression ReaderValue { get { return reader.Param; } }
        public int HierarchyDepth { get { return hierarchyDepth; } }
        public bool IsBonded { get { return schema.IsBonded; } }

        public Expression Apply(ITransform transform)
        {
            Debug.Assert(schema.IsStruct);

            var body = new List<Expression>
            {
                transform.Begin
            };

            if (schema.HasBase)
                body.Add(transform.Base(new UntaggedParser<R>(this, schema.GetBaseSchema())));

            // Performs left outer join of schema fields with transform fields.
            // The result contains entry for each schema field. For fields not handled
            // by the transform default to Skip.
            body.AddRange(
                from fieldDef in schema.StructDef.fields
                join transfromField in transform.Fields on fieldDef.id equals transfromField.Id into fields
                from knownField in fields.DefaultIfEmpty()
                select Field(transform, fieldDef, knownField));

            body.Add(transform.End);
            return Expression.Block(body);
        }

        Expression Field(ITransform transform, FieldDef fieldDef, IField field)
        {
            var fieldId = Expression.Constant(fieldDef.id);
            var fieldType = Expression.Constant(fieldDef.type.id);
            var fieldParser = new UntaggedParser<R>(this, schema.GetFieldSchema(fieldDef));

            return Expression.IfThenElse(reader.ReadFieldOmitted(),
                field != null ? field.Omitted : Expression.Empty(),
                field != null ?
                    field.Value(fieldParser, fieldType) :
                    transform.UnknownField(fieldParser, fieldType, fieldId) ?? fieldParser.Skip(fieldType));
        }

        public Expression Container(BondDataType? expectedType, ContainerHandler handler)
        {
            Debug.Assert(schema.IsContainer);

            var count = Expression.Variable(typeof(int), "count");

            var loop = handler(
                new UntaggedParser<R>(this, schema.GetElementSchema()),
                Expression.Constant(schema.TypeDef.element.id),
                Expression.GreaterThan(Expression.PostDecrementAssign(count), Expression.Constant(0)),
                count,
                null);

            return Expression.Block(
                new[] { count },
                Expression.Assign(count, reader.ReadContainerBegin()),
                loop,
                reader.ReadContainerEnd());
        }

        public Expression Map(BondDataType? expectedKeyType, BondDataType? expectedValueType, MapHandler handler)
        {
            Debug.Assert(schema.IsMap);

            var count = Expression.Variable(typeof(int), "count");

            var loop = handler(
                new UntaggedParser<R>(this, schema.GetKeySchema()),
                new UntaggedParser<R>(this, schema.GetElementSchema()),
                Expression.Constant(schema.TypeDef.key.id),
                Expression.Constant(schema.TypeDef.element.id),
                Expression.GreaterThan(Expression.PostDecrementAssign(count), Expression.Constant(0)),
                Expression.Empty(),
                count);

            return Expression.Block(
                new[] { count },
                Expression.Assign(count, reader.ReadContainerBegin()),
                loop,
                reader.ReadContainerEnd());
        }

        public Expression Blob(Expression count)
        {
            return reader.ReadBytes(count);
        }

        public Expression Scalar(Expression valueType, BondDataType expectedType, ValueHandler handler)
        {
            Debug.Assert(valueType is ConstantExpression);

            return handler(reader.Read((BondDataType)(valueType as ConstantExpression).Value));
        }

        public Expression Bonded(ValueHandler handler)
        {
            if (schema.IsBonded)
            {
                return handler(reader.ReadMarshaledBonded());
            }

            var newBonded = bondedFactory(reader.Param, Expression.Constant(schema));

            return Expression.Block(
                handler(newBonded),
                SkipStruct());
        }

        public Expression Skip(Expression valueType)
        {
            Debug.Assert(valueType is ConstantExpression);
            var dataType = (BondDataType)(valueType as ConstantExpression).Value;
            Debug.Assert(schema.TypeDef.id == dataType);

            switch (dataType)
            {
                case BondDataType.BT_SET:
                    return SkipSet();

                case BondDataType.BT_LIST:
                    return SkipList();

                case BondDataType.BT_MAP:
                    return SkipMap();

                case BondDataType.BT_STRUCT:
                    return SkipStruct();

                default:
                    return reader.Skip(dataType);
            }
        }

        static Expression NewBonded(Expression reader, Expression schema)
        {
            var ctor =
                typeof(BondedVoid<>).MakeGenericType(reader.Type).GetConstructor(reader.Type, typeof(RuntimeSchema));

            return Expression.New(ctor, reader, schema);
        }

        Expression SkipSet()
        {
            return Container(null, (valueParser, elementType, next, count, arraySegment) =>
                ControlExpression.While(next, valueParser.Skip(elementType)));
        }

        Expression SkipList()
        {
            return Container(null, (valueParser, elementType, next, count, arraySegment) =>
            {
                Debug.Assert(elementType is ConstantExpression);
                var elementDataType = (BondDataType)(elementType as ConstantExpression).Value;

                if (elementDataType == BondDataType.BT_UINT8 || elementDataType == BondDataType.BT_INT8)
                    return reader.SkipBytes(count);
                else
                    return ControlExpression.While(next, valueParser.Skip(elementType));
            });
        }

        Expression SkipMap()
        {
            return Map(null, null, (keyParser, valueParser, keyType, valueType, nextKey, nextValue, count) =>
                ControlExpression.While(nextKey,
                    Expression.Block(
                        keyParser.Skip(keyType),
                        valueParser.Skip(valueType))));
        }

        Expression SkipStruct()
        {
            if (deferredSkip.InProgress.Contains(schema))
            {
                int index;
                if (!deferredSkip.Index.TryGetValue(schema, out index))
                {
                    index = deferredSkip.Lambdas.Count;
                    deferredSkip.Index[schema] = index;
                    deferredSkip.Lambdas.Add(null);
                    deferredSkip.Lambdas[index] = Expression.Lambda<Action<R>>(
                        Apply(skipStructTransform), reader.Param).Compile();
                }
                var lambdas = deferredSkip.Lambdas;
                return SkipStruct(r => lambdas[index](r));
            }

            deferredSkip.InProgress.Add(schema);
            var skip = Apply(skipStructTransform);
            deferredSkip.InProgress.Remove(schema);
            return skip;
        }

        Expression SkipStruct(Expression<Action<R>> skip)
        {
            return Expression.Invoke(skip, reader.Param);
        }

        public override bool Equals(object that)
        {
            Debug.Assert(that is UntaggedParser<R>);
            return Comparer.Equal(schema.TypeDef, (that as UntaggedParser<R>).schema.TypeDef);
        }

        public override int GetHashCode()
        {
            return schema.TypeDef.CalculateHashCode();
        }
    }
}
