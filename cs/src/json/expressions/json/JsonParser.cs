// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Json
{
    using System.Linq.Expressions;
    
    using Bond.Expressions.Pull;
    using Bond.Protocols;

    using Newtonsoft.Json;

    public abstract class JsonParser<R> : PullParser<JsonToken> where R : IJsonReader
    {
        readonly JsonReader<R> reader = new JsonReader<R>();

        protected JsonParser(RuntimeSchema schema, bool flatten)
            : base(schema, flatten)
        {
        }

        protected JsonParser(JsonParser<R> that, RuntimeSchema schema, bool flatten)
            : base(that, schema, flatten)
        {
            reader = that.reader;
        }

        protected JsonReader<R> Reader { get { return reader; } }

        #region IParser Implementation

        public override ParameterExpression ReaderParam { get { return reader.Param; } }

        public override Expression ReaderValue { get { return reader.Param; } }

        protected override Expression Read() { return reader.Read(); }

        protected override Expression EOF { get { return reader.EOF; } }

        protected override Expression Token() { return reader.TokenType; }

        #endregion
    }
}
