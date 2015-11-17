// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Xml
{
    using System.Linq.Expressions;
    using System.Xml;

    using Bond.Expressions.Pull;
    using Bond.Protocols;

    public abstract class XmlParser<R> : PullParser<XmlNodeType> where R : IXmlReader
    {
        readonly XmlReader<R> reader = new XmlReader<R>();

        protected XmlParser(RuntimeSchema schema, bool flatten) : base(schema, flatten)
        {
        }

        protected XmlParser(XmlParser<R> that, RuntimeSchema schema, bool flatten) : base(that, schema, flatten)
        {
            reader = that.reader;
        }

        protected XmlReader<R> Reader { get { return reader; } }

        #region IParser Implementation

        public override ParameterExpression ReaderParam { get { return reader.Param; } }
        
        public override Expression ReaderValue { get { return reader.Param; } }

        protected override Expression Read() {return reader.Read(); }

        protected override Expression EOF { get { return reader.EOF; } }

        protected override Expression Token() { return reader.NodeType; }

        #endregion
    }
}
