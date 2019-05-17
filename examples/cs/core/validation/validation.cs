namespace Examples
{
    using Bond;
    using Bond.Expressions;
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using System.Linq.Expressions;

    public class SchemaValidation<R>
    {
        internal readonly Action<R>[] validate;

        public SchemaValidation(RuntimeSchema schema)
        {
            var transform = new ValidatorTransform<R>((r, i) => validate[i](r));

            validate = transform.Generate(ParserFactory<R>.Create(schema), schema).Select(lambda => lambda.Compile()).ToArray();
        }

        public void Validate(R reader)
        {
            validate[0](reader);
        }

    }

    class ValidatorTransform<R>
    {
        static readonly Expression<Action<string, ushort>> throwUnknownFieldException =
            (s, f) => ThrowUnknownFieldException(s, f);

        public static Expression UnknownFieldException(string schema, Expression tag)
        {
            return Expression.Invoke(throwUnknownFieldException, Expression.Constant(schema), tag);
        }

        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        static void ThrowUnknownFieldException(string schema, ushort tag)
        {
            throw new System.IO.InvalidDataException(string.Format("Unexpected tag #{1} in schema {0}.", schema, tag));
        }

        readonly List<Expression<Action<R>>> validateActions = new List<Expression<Action<R>>>();

        readonly Dictionary<RuntimeSchema, Validate> validateDelegates =
            new Dictionary<RuntimeSchema, Validate>(new TypeDefComparer());

        readonly Expression<Action<R, int>> deferredValidate;

        readonly Dictionary<KeyValuePair<IParser, Validate>, int> validateIndex = new Dictionary<KeyValuePair<IParser, Validate>, int>();

        readonly Stack<KeyValuePair<IParser, Validate>> inProgress = new Stack<KeyValuePair<IParser, Validate>>();

        protected delegate Expression Validate(IParser parser);
        protected delegate Expression ValidateWithSchema(IParser parser, RuntimeSchema schema);

        static readonly Expression noMetadata = Expression.Constant(null, typeof(Metadata));


        public ValidatorTransform(Expression<Action<R, int>> deferredValidate)
        {
            this.deferredValidate = deferredValidate;
        }

        public IEnumerable<Expression<Action<R>>> Generate(IParser parser, RuntimeSchema schema)
        {
            GenerateValidate(Struct, parser, schema);
            return validateActions;
        }

        Expression GenerateValidate(ValidateWithSchema validateWithSchema, IParser parser, RuntimeSchema schema)
        {
            Validate validate;
            if (!validateDelegates.TryGetValue(schema, out validate))
            {
                validate = validateDelegates[schema] = p => validateWithSchema(p, schema);
            }

            return GenerateValidate(validate, parser);
        }

        Expression GenerateValidate(Validate validate, IParser parser)
        {
            var key = new KeyValuePair<IParser, Validate>(parser, validate);

            Expression body;

            inProgress.Push(key);

            int index;
            if (!validateIndex.TryGetValue(key, out index))
            {
                index = validateActions.Count;
                validateIndex[key] = index;
                validateActions.Add(null);
                validateActions[index] = Expression.Lambda<Action<R>>(
                    validate(parser),
                    parser.ReaderParam);
            }

            body = Expression.Invoke(
                deferredValidate,
                PrunedExpression.Convert(parser.ReaderValue, parser.ReaderParam.Type),
                Expression.Constant(index));


            inProgress.Pop();


            return body;
        }

        Expression Struct(IParser parser, RuntimeSchema schema)
        {
            var metadata = schema.HasValue ? Expression.Constant(schema.StructDef.metadata) : noMetadata;
            var baseSchema = schema.HasBase ? schema.GetBaseSchema() : RuntimeSchema.Empty;

            return parser.Apply(new Transform(
                Fields: schema.HasValue ?
                    (from field in schema.StructDef.fields
                     select new Field(
                         Id: field.id,
                         Value: (fieldParser, fieldType) =>
                             Expression.Block(
                                 Value(fieldParser, fieldType, schema.GetFieldSchema(field))),
                         Omitted: () => field.metadata.modifier == Modifier.Required ?
                                 ThrowExpression.RequiredFieldMissingException(schema.StructDef.metadata.name, Expression.Constant(field.metadata.name))
                                 : Expression.Empty()
                             ))
                    : null,
                UnknownField: (fieldParser, fieldType, fieldId) =>
                    UnknownFieldException(schema.StructDef.metadata.name, fieldId),
                Base: baseParser =>
                    baseSchema.HasValue ? Struct(baseParser, baseSchema) : Expression.Empty()
                ));
        }

        Expression Value(IParser parser, Expression valueType, RuntimeSchema schema)
        {
            Debug.Assert(schema.HasValue);

            if (parser.IsBonded /*|| (untaggedWriter && schema.IsBonded)*/)
                return parser.Bonded(value => value);

            if (schema.IsStruct)
                return GenerateValidate(Struct, parser, schema);

            if (schema.IsMap)
                return GenerateValidate(Map, parser, schema);

            if (schema.IsContainer)
                return GenerateValidate(Container, parser, schema);

            return parser.Scalar(valueType, schema.TypeDef.id,
                value => value);
        }

        Expression Map(IParser parser, RuntimeSchema schema)
        {
            var expectedValueType = schema.HasValue ? schema.TypeDef.element.id : (BondDataType?)null;
            var expectedKeyType = schema.HasValue ? schema.TypeDef.key.id : (BondDataType?)null;

            return parser.Map(expectedKeyType, expectedValueType,
                (keyParser, valueParser, keyType, valueType, nextKey, nextValue, count) =>
                Expression.Block(
                        ControlExpression.While(nextKey,
                            Expression.Block(
                                    Value(keyParser, keyType, schema.GetKeySchema()),
                                nextValue,
                                    Value(valueParser, valueType, schema.GetElementSchema())
                                ))
                    ));
        }

        Expression Container(IParser parser, RuntimeSchema schema)
        {
            var expectedValueType = schema.HasValue ? schema.TypeDef.element.id : (BondDataType?)null;

            return parser.Container(expectedValueType,
                (valueParser, elementType, next, count, arraySegment) =>
                {
                    var body = ControlExpression.While(next,
                        Expression.Block(
                                Value(valueParser, elementType, schema.GetElementSchema())
                            ));

                    return body;
                });
        }
    }

}
