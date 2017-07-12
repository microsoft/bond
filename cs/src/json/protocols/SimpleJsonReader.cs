// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;
    using System.IO;
    using System.Runtime.CompilerServices;
    
    using Bond.Expressions.Json;
    
    using Newtonsoft.Json;

    [Parser(typeof(SimpleJsonParser<>))]
    public struct SimpleJsonReader : IJsonReader
    {
        readonly JsonTextReader reader;
        bool eof;
        
        public SimpleJsonReader(TextReader reader)
        {
            this.reader = new JsonTextReader(reader);
            this.reader.DateParseHandling = DateParseHandling.None;
            this.reader.FloatParseHandling = FloatParseHandling.Double;

            eof = false;
        }

        public SimpleJsonReader(Stream stream)
        {
            reader = new JsonTextReader(new StreamReader(stream));
            reader.DateParseHandling = DateParseHandling.None;
            reader.FloatParseHandling = FloatParseHandling.Double;
            eof = false;
        }

        public bool EOF
        {
            get { return eof; }
        }

        public void Skip()
        {
            reader.Skip();
        }

        public JsonToken TokenType
        {
            get { return reader.TokenType; }
        }

        public Type ValueType
        {
            get { return reader.ValueType; }
        }

        public object Value
        {
            get { return reader.Value; }
        }

        public int LineNumber
        {
            get { return reader.LineNumber; }
        }

        public int LinePosition
        {
            get { return reader.LinePosition; }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Read()
        {
            eof = !reader.Read();
        }
    }
}
