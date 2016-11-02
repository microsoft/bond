// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Xml
{
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Protocols;

    public class XmlReader<R> where R : IXmlReader
    {
        static readonly MethodInfo read = BondReflection.MethodInfoOf((IXmlReader r) => r.Read());
        static readonly MethodInfo skip = BondReflection.MethodInfoOf((IXmlReader r) => r.Skip());
        static readonly PropertyInfo eof = BondReflection.PropertyInfoOf((IXmlReader r) => r.EOF);
        static readonly PropertyInfo nodeType = BondReflection.PropertyInfoOf((IXmlReader r) => r.NodeType);
        static readonly PropertyInfo localName = BondReflection.PropertyInfoOf((IXmlReader r) => r.LocalName);
        static readonly PropertyInfo namespaceUri = BondReflection.PropertyInfoOf((IXmlReader r) => r.NamespaceURI);
        static readonly PropertyInfo isEmptyElement = BondReflection.PropertyInfoOf((IXmlReader r) => r.IsEmptyElement);
        static readonly PropertyInfo value = BondReflection.PropertyInfoOf((IXmlReader r) => r.Value);

        readonly ParameterExpression reader = Expression.Parameter(typeof(R), "reader");

        public ParameterExpression Param { get { return reader; } }

        public Expression EOF { get { return Expression.Property(reader, eof); } }

        public Expression NodeType { get { return Expression.Property(reader, nodeType); } }

        public Expression LocalName { get { return Expression.Property(reader, localName); } }

        public Expression NamespaceURI { get { return Expression.Property(reader, namespaceUri); } }
        
        public Expression Read() { return Expression.Call(reader, read); }

        public Expression Skip() { return Expression.Call(reader, skip); }

        public Expression IsEmptyElement { get { return Expression.Property(reader, isEmptyElement); } }

        public Expression Value { get { return Expression.Property(reader, value); } }
    }
}
