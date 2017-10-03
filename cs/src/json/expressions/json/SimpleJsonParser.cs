// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Json
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Globalization;
    using System.Linq.Expressions;

#if SUPPORTS_BIGINTEGER
    using System.Numerics;
#endif

    using Bond.Expressions.Pull;
    using Bond.Protocols;

    using Newtonsoft.Json;

    public class SimpleJsonParser<R> : JsonParser<R>
        where R : IJsonReader
    {
        static readonly Dictionary<BondDataType, JsonToken> ScalarTokenTypes = new Dictionary<BondDataType, JsonToken>
        {
            { BondDataType.BT_BOOL, JsonToken.Boolean },
            { BondDataType.BT_FLOAT, JsonToken.Float },
            { BondDataType.BT_DOUBLE, JsonToken.Float },
            { BondDataType.BT_INT8, JsonToken.Integer },
            { BondDataType.BT_INT16, JsonToken.Integer },
            { BondDataType.BT_INT32, JsonToken.Integer },
            { BondDataType.BT_INT64, JsonToken.Integer },
            { BondDataType.BT_UINT8, JsonToken.Integer },
            { BondDataType.BT_UINT16, JsonToken.Integer },
            { BondDataType.BT_UINT32, JsonToken.Integer },
            { BondDataType.BT_UINT64, JsonToken.Integer },
            { BondDataType.BT_STRING, JsonToken.String },
            { BondDataType.BT_WSTRING, JsonToken.String },
        };

        public SimpleJsonParser(RuntimeSchema schema)
            : base(schema, flatten: true)
        {
        }

        public SimpleJsonParser(Type type)
            : base(Bond.Schema.GetRuntimeSchema(type), flatten: true)
        {
        }

        protected SimpleJsonParser(SimpleJsonParser<R> that, RuntimeSchema schema)
            : base(that, schema, flatten: true)
        {
        }

        public override Expression Container(BondDataType? expectedType, ContainerHandler handler)
        {
            var parser = new SimpleJsonParser<R>(this, Schema.GetElementSchema());
            var elementType = Expression.Constant(Schema.TypeDef.element.id);

            // if the json contains null instead of an array, simulate an empty list to deserialize a null value
            // into a nullable
            var readJsonNullAsEmptyList = Expression.Block(
                Reader.Read(),
                handler(parser, elementType, Expression.Constant(false), Expression.Constant(0), null));

            // if the json contains an array, read the array into the list
            var readJsonArrayAsList = Expression.Block(
                Reader.Read(),
                handler(parser, elementType, JsonTokenNotEquals(JsonToken.EndArray), Expression.Constant(0), null),
                Reader.Read());

            return Expression.IfThenElse(
                JsonTokenEquals(JsonToken.Null),
                readJsonNullAsEmptyList,
                Expression.IfThenElse(
                    JsonTokenEquals(JsonToken.StartArray),
                    readJsonArrayAsList,
                    ThrowUnexpectedInput("Expected JSON array or null.")));
        }

        public override Expression Map(BondDataType? expectedKeyType, BondDataType? expectedValueType, MapHandler handler)
        {
            var keyParser = new SimpleJsonParser<R>(this, Schema.GetKeySchema());
            var valueParser = new SimpleJsonParser<R>(this, Schema.GetElementSchema());
            var keyType = Expression.Constant(Schema.TypeDef.key.id);
            var valueType = Expression.Constant(Schema.TypeDef.element.id);

            var next = JsonTokenNotEquals(JsonToken.EndArray);

            return Expression.Block(
                Reader.Read(),
                handler(keyParser, valueParser, keyType, valueType, next, next, Expression.Constant(0)),
                Reader.Read());
        }

        public override Expression Blob(Expression count)
        {
            // null means that blobs will be handled as byte arrays
            return null;
        }

        public override Expression Scalar(Expression valueType, BondDataType expectedType, ValueHandler handler)
        {
            JsonToken scalarTokenType;
            if (!ScalarTokenTypes.TryGetValue(expectedType, out scalarTokenType))
            {
                Debug.Assert(false, "Scalar should be called only on scalar expected types.");
            }

            Expression convertedValue;

            if (scalarTokenType == JsonToken.Integer)
            {
                // reader.Value is a boxed long and must be unboxed to the right type in order to 
                // avoid an InvalidCastException.
                convertedValue = Expression.Convert(Reader.Value, typeof(long));
            }
            else if (scalarTokenType == JsonToken.Float)
            {
                convertedValue = Expression.Convert(Reader.Value, typeof(double));
            }
            else
            {
                convertedValue = Reader.Value;
            }

            var errorMessage = 
                StringExpression.Format(
                    "Invalid input, expected JSON token of type {0}, encountered {1}",
                    Expression.Constant(scalarTokenType, typeof(object)),
                    Expression.Convert(Reader.TokenType, typeof(object)));

            Expression embeddedExpression = handler(convertedValue);

#if SUPPORTS_BIGINTEGER
            if (expectedType == BondDataType.BT_UINT64 && scalarTokenType == JsonToken.Integer)
            {
                embeddedExpression =
                    Expression.IfThenElse(
                        Expression.TypeIs(Reader.Value, typeof(long)),
                        embeddedExpression,
                        handler(Expression.Convert(Reader.Value, typeof(BigInteger))));
            }
#endif

            var handleValue =
                Expression.IfThenElse(
                    JsonTokenEquals(scalarTokenType),
                    embeddedExpression,
                    ThrowUnexpectedInput(errorMessage));

            // If a floating point value is expected also accept an integer
            if (scalarTokenType == JsonToken.Float)
            {
                handleValue = Expression.IfThenElse(
                    JsonTokenEquals(JsonToken.Integer),
                    handler(Expression.Convert(Reader.Value, typeof(long))),
                    handleValue);
            }

            return
                Expression.Block(
                    handleValue,
                    Reader.Read());
        }

        protected override IStateMachine<JsonToken> CreateStateMachine(IEnumerable<TransformSchemaPair> transforms, ParameterExpression requiredFields)
        {
            return new StateMachine<JsonToken>
            {
                InitialState = State.BeforeStructObject,
                FinalState = State.Finished,
                Default = ThrowUnexpectedState,
                TokenTransitions = new[]
                {
                    new TokenTransition<JsonToken>
                    {
                        Token = JsonToken.None,
                        Default = ThrowUnexpectedState,
                        StateTransitions = new[]
                        {
                            new StateTransition(State.BeforeStructObject, state => Reader.Read())
                        }
                    },
                    new TokenTransition<JsonToken>
                    {
                        Token = JsonToken.StartObject,
                        Default = ThrowUnexpectedState,
                        StateTransitions = new[]
                        {
                            new StateTransition(State.BeforeStructObject, State.InStructObject, state => Reader.Read())
                        }
                    },
                    new TokenTransition<JsonToken>
                    {
                        Token = JsonToken.EndObject,
                        StateTransitions = new[]
                        {
                            new StateTransition(State.InStructObject, State.Finished, state => Reader.Read())
                        }
                    },
                    new TokenTransition<JsonToken>
                    {
                        Token = JsonToken.PropertyName,
                        Default = ThrowUnexpectedState,
                        StateTransitions = new[]
                        {
                            new StateTransition(State.InStructObject, state => ProcessField(requiredFields, transforms))
                        }
                    }
                }
            };
        }

        Expression ThrowUnexpectedState(Expression state)
        {
            return ThrowExpression.InvalidDataException(
                StringExpression.Format(
                    "Unexpected JsonToken '{0}' at state {1} (line {2} position {3})",
                    Expression.Convert(Reader.TokenType, typeof(object)),
                    Expression.Convert(state, typeof(object)),
                    Expression.Convert(Reader.LineNumber, typeof(object)),
                    Expression.Convert(Reader.LinePosition, typeof(object))));
        }

        Expression ThrowUnexpectedInput(Expression errorMessage)
        {
            return ThrowExpression.InvalidDataException(
                StringExpression.Format(
                    "{0} (line {1} position {2})",
                    errorMessage,
                    Expression.Convert(Reader.LineNumber, typeof(object)),
                    Expression.Convert(Reader.LinePosition, typeof(object))));
        }

        Expression ThrowUnexpectedInput(string errorMessage)
        {
            return ThrowUnexpectedInput(Expression.Constant(errorMessage));
        }

        Expression ProcessField(ParameterExpression requiredFields, IEnumerable<TransformSchemaPair> transforms)
        {
            // unknown fields are skipped (read past the unknown PropertyName then skip the value)
            Expression body = Expression.Block(
                Reader.Read(),
                SkipValue());

            var requiredIndex = 0;

            foreach (var pair in transforms)
            {
                var currentSchema = pair.Schema;
                var currentTransform = pair.Transform;
                
                var structDef = currentSchema.StructDef;
                var index = 0;

                foreach (var field in currentTransform.Fields)
                {
                    var fieldDef = structDef.fields[index++];
                    Debug.Assert(field.Id == fieldDef.id);

                    var parser = new SimpleJsonParser<R>(this, currentSchema.GetFieldSchema(fieldDef));

                    // chain ifs on field name to handle the value on the right field handler
                    var handleField = field.Value(parser, Expression.Constant(fieldDef.type.id));

                    if (fieldDef.metadata.modifier == Modifier.Required)
                    {
                        handleField = Expression.Block(RequiredFields.Mark(requiredFields, requiredIndex++), handleField);
                    }

                    string name;
                    body = Expression.IfThenElse(
                        PropertyNameEquals(fieldDef.metadata.attributes.TryGetValue(
                            SimpleJsonWriter.NameAttribute, out name) ? name : fieldDef.metadata.name),
                        Expression.Block(Read(), handleField),
                        body);
                }
            }

            return body;
        }

        Expression PropertyNameEquals(string name)
        {
            return Expression.Equal(Expression.Convert(Reader.Value, typeof(string)), Expression.Constant(name));
        }

        Expression JsonTokenEquals(JsonToken token)
        {
            return Expression.Equal(Reader.TokenType, Expression.Constant(token));
        }

        Expression JsonTokenNotEquals(JsonToken token)
        {
            return Expression.NotEqual(Reader.TokenType, Expression.Constant(token));
        }

        Expression SkipValue()
        {
            return
                Expression.Block(
                    Expression.IfThen(
                        Expression.OrElse(
                            Expression.Equal(Reader.TokenType, Expression.Constant(JsonToken.StartObject)),
                            Expression.Equal(Reader.TokenType, Expression.Constant(JsonToken.StartArray))),
                        Reader.Skip()),
                    Reader.Read());
        }

        static class State
        {
            public const byte BeforeStructObject = 1;
            public const byte InStructObject = 2;
            public const byte Finished = 5;
        }
    }
}
