// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Xml
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Globalization;
    using System.IO;
    using System.Linq.Expressions;
    using System.Runtime.CompilerServices;
    using System.Xml;

    using Bond.Expressions;
    using Bond.Expressions.Pull;
    using Bond.Protocols;

    public class SimpleXmlParser<R> : XmlParser<R> where R : IXmlReader
    {
        static readonly Expression<Action<byte, XmlNodeType, string, string>> parsingError =
            (s, t, n, v) => ParsingError(s, t, n, v);

        static readonly Expression<Action<XmlNodeType>> unexpectedNodeError = n => UnexpectedNodeError(n);

        delegate Expression ContainerItemHandler(Expression nextItem);

        public SimpleXmlParser(RuntimeSchema schema)
            : base(schema, flatten: true)
        {}

        public SimpleXmlParser(Type type)
            : base(Bond.Schema.GetRuntimeSchema(type), flatten: true)
        {}

        SimpleXmlParser(XmlParser<R> that, RuntimeSchema schema)
            : base(that, schema, flatten: true)
        {}

        protected override IStateMachine<XmlNodeType> CreateStateMachine(
            IEnumerable<TransformSchemaPair> transforms,
            ParameterExpression requiredFields)
        {
            return new StateMachine<XmlNodeType>
            {
                InitialState = State.AtStructElement,
                FinalState = State.Finished,
                IgnoredTokens = new[] { XmlNodeType.Comment, XmlNodeType.Text, XmlNodeType.Whitespace, XmlNodeType.XmlDeclaration, },
                Default = state => Expression.Invoke(unexpectedNodeError, Reader.NodeType),
                TokenTransitions = new[]
                    {
                        new TokenTransition<XmlNodeType>
                        {
                            Token = XmlNodeType.Element,
                            StateTransitions = new[]
                            {
                                new StateTransition(State.AtStructElement,  
                                    ProcessStructElement),
                                new StateTransition(State.InsideStructElement, 
                                    state => ProcessFieldElement(state, requiredFields, transforms)),
                                new StateTransition(State.AtFieldEndElement, State.InsideStructElement, 
                                    state => Reader.Read())
                            },
                            Default = state => ParsingError(state),
                        },
                        new TokenTransition<XmlNodeType>
                        {
                            Token = XmlNodeType.EndElement,
                            StateTransitions = new[]
                            {
                                new StateTransition(State.AtFieldEndElement, State.InsideStructElement, 
                                    state => Reader.Read()),
                                new StateTransition(State.InsideStructElement, State.Finished, 
                                    state => Reader.Read())
                            },
                            Default = state => ParsingError(state),
                        }
                    }
            };
        }

        Expression ProcessStructElement(Expression state)
        {
            return Expression.Block(
                Expression.IfThenElse(
                    Reader.IsEmptyElement,
                    Expression.Assign(state, Expression.Constant(State.Finished)),
                    Expression.Assign(state, Expression.Constant(State.InsideStructElement))),
                Reader.Read());
        }
        
        Expression ProcessFieldElement(
            Expression state, 
            ParameterExpression requiredFields, 
            IEnumerable<TransformSchemaPair> transforms)
        {
            var requiredIndex = 0;

            // start from the expression to handle unknown element (will be executed at the end of the if/else chain)
            Expression body = Expression.Block(
                Reader.Skip(),
                Expression.Assign(state, Expression.Constant(State.InsideStructElement)));
            
            // if there are transform/schema pairs for base structs, process their fields as well into the expression
            foreach (var pair in transforms)
            {
                var index = 0;
                var structDef = pair.Schema.StructDef;

                foreach (var field in pair.Transform.Fields)
                {
                    var fieldDef = structDef.fields[index++];
                    Debug.Assert(field.Id == fieldDef.id);

                    var parser = new SimpleXmlParser<R>(this, pair.Schema.GetFieldSchema(fieldDef));

                    // process field - and set the state to expect to see the field element's end
                    var handleField = new List<Expression>
                    {
                        field.Value(parser, Expression.Constant(fieldDef.type.id)),
                        Expression.Assign(state, Expression.Constant(State.AtFieldEndElement))
                    };

                    if (fieldDef.metadata.modifier == Modifier.Required)
                    {
                        handleField.Add(RequiredFields.Mark(requiredFields, requiredIndex++));
                    }

                    body = Expression.IfThenElse(
                        NodeNameEquals(fieldDef.metadata.name, structDef.metadata.GetXmlNamespace()),
                        Expression.Block(handleField),
                        body);
                }
            }

            return body;
        }
        
        public override Expression Apply(ITransform transform)
        {
            // in SimpleXml protocol, the parser is expecting to read one step in order to get to the actual
            // element that represents the struct
            return Expression.Block(
                Read(),
                base.Apply(transform));
        }

        public override Expression Blob(Expression count)
        {
            // TODO: for now handle blob as array of bytes; consider CDATA
            return null;
        }

        public override Expression Scalar(Expression valueType, BondDataType expectedType, ValueHandler handler)
        {
            var stringValue = Expression.Variable(typeof(string), "valueAsString");
            
            // Reading primitive field of a struct, or a primitive list item - value is in Xml element text.
            // If we have an empty element, parse it as if it has an empty text.
            // Otherwise: read once to get to the text, handle the text, read once more to get to the end element.
            return Expression.Block(
                new[] { stringValue },
                Expression.IfThenElse(
                    Reader.IsEmptyElement,
                    Expression.Assign(stringValue, StringExpression.Empty()),
                    Expression.Block(
                        Reader.Read(),
                        Expression.IfThenElse(
                            Expression.Equal(Reader.NodeType, Expression.Constant(XmlNodeType.EndElement)),
                            Expression.Assign(stringValue, Expression.Constant(string.Empty)),
                            Expression.Block(
                                IfNotNodeType(XmlNodeType.Text, IfNotNodeType(XmlNodeType.Whitespace, ParsingError())),
                                Expression.Assign(stringValue, Reader.Value),
                                Reader.Read(),
                                IfNotNodeType(XmlNodeType.EndElement, ParsingError()))))),
                handler(StringExpression.Convert(stringValue, expectedType)));
        }

        public override Expression Container(BondDataType? expectedType, ContainerHandler handler)
        {
            return Items(nextItem => handler(
                new SimpleXmlParser<R>(this, Schema.GetElementSchema()), 
                Expression.Constant(Schema.TypeDef.element.id), 
                nextItem, 
                Expression.Constant(0),
                null));
        }

        public override Expression Map(BondDataType? expectedKeyType, BondDataType? expectedValueType, MapHandler handler)
        {
            return Items(nextItem => handler(
                new SimpleXmlParser<R>(this, Schema.GetKeySchema()), 
                new SimpleXmlParser<R>(this, Schema.GetElementSchema()),
                Expression.Constant(Schema.TypeDef.key.id), 
                Expression.Constant(Schema.TypeDef.element.id), 
                nextItem,
                nextItem,
                Expression.Constant(0)));
        }

        Expression Items(ContainerItemHandler handler)
        {
            // If the list is an empty element we won't read anything
            var isEmpty = Expression.Variable(typeof(bool), "isEmpty");

            // Generate the following code for the "next" expression:
            //  if (isEmpty)
            //  {
            //      return false;
            //  }
            //  else 
            //  {
            //      while (reader.NodeType == XmlNodeType.Whitespace) reader.Read;
            //      do { reader.Read(); } while (reader.NodeType == XmlNodeType.Whitespace;
            //
            //      if (reader.NodeType == XmlNodeType.Element) &&
            //          reader.LocalName != "Item")
            //      {
            //          throw new InvalidDataException();
            //      }
            //      
            //      return reader.XmlNode != XmlNodeType.EndElement;
            //  }

            var whitespace = Expression.Equal(Reader.NodeType, Expression.Constant(XmlNodeType.Whitespace));

            var next = Expression.Condition(isEmpty,
                Expression.Constant(false),
                Expression.Block(
                    ControlExpression.While(whitespace, Reader.Read()),
                    ControlExpression.DoWhile(Reader.Read(), whitespace),
                    Expression.IfThen(
                        Expression.AndAlso(
                            Expression.Equal(Reader.NodeType, Expression.Constant(XmlNodeType.Element)),
                            Expression.NotEqual(Reader.LocalName, Expression.Constant("Item"))),
                        ParsingError()),
                    Expression.NotEqual(Reader.NodeType, Expression.Constant(XmlNodeType.EndElement))));

            return Expression.Block(
                new[] { isEmpty },
                Expression.Assign(isEmpty, Reader.IsEmptyElement),
                handler(next));
        }

        Expression IfNotNodeType(XmlNodeType type, Expression then)
        {
            return Expression.IfThen(
                Expression.NotEqual(Reader.NodeType, Expression.Constant(type)),
                then);
        }

        Expression NodeNameEquals(string localName, string namespaceUri)
        {
            if (string.IsNullOrEmpty(namespaceUri))
            {
                return StringExpression.Equals(Reader.LocalName, localName, StringComparison.OrdinalIgnoreCase);
            }

            return Expression.AndAlso(
                StringExpression.Equals(Reader.LocalName, localName, StringComparison.OrdinalIgnoreCase),
                Expression.OrElse(
                    StringExpression.Equals(Reader.NamespaceURI, StringExpression.Empty(), StringComparison.OrdinalIgnoreCase),
                    StringExpression.Equals(Reader.NamespaceURI, namespaceUri, StringComparison.OrdinalIgnoreCase)));
        }

        static void UnexpectedNodeError(XmlNodeType type)
        {
            throw new InvalidDataException(string.Format(CultureInfo.InvariantCulture, "Unexpected node type: {0}", type));
        }

        Expression ParsingError(Expression state)
        {
            return Expression.Invoke(parsingError, state, Reader.NodeType, Reader.LocalName, Reader.Value);
        }

        Expression ParsingError()
        {
            return Expression.Invoke(parsingError, Expression.Constant((byte)0), Reader.NodeType, Reader.LocalName, Reader.Value);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void ParsingError(byte state, XmlNodeType type, string name, string value)
        {
            throw new InvalidDataException(string.Format(CultureInfo.InvariantCulture,
                "Parsing error: state '{0}', (Xml node type='{1}', name='{2}', value='{3}')",
                state, type, name, value));
        }

        static class State
        {
            public const byte AtStructElement = 1;
            public const byte InsideStructElement = 2;
            public const byte AtFieldEndElement = 3;
            public const byte Finished = 4;
        }
    }
}
