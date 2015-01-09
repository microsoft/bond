// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Json
{
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Protocols;

    public class JsonReader<R> where R : IJsonReader
    {
        static readonly MethodInfo read = Reflection.MethodInfoOf((IJsonReader r) => r.Read());
        static readonly MethodInfo skip = Reflection.MethodInfoOf((IJsonReader r) => r.Skip());
        static readonly PropertyInfo eof = Reflection.PropertyInfoOf((IJsonReader r) => r.EOF);
        static readonly PropertyInfo tokenType = Reflection.PropertyInfoOf((IJsonReader r) => r.TokenType);
        static readonly PropertyInfo lineNumber = Reflection.PropertyInfoOf((IJsonReader r) => r.LineNumber);
        static readonly PropertyInfo linePosition = Reflection.PropertyInfoOf((IJsonReader r) => r.LinePosition);
        static readonly PropertyInfo value = Reflection.PropertyInfoOf((IJsonReader r) => r.Value);
        
        readonly ParameterExpression reader = Expression.Parameter(typeof(R), "reader");

        public ParameterExpression Param { get { return reader; } }

        public Expression EOF { get { return Expression.Property(reader, eof); } }

        public Expression TokenType { get { return Expression.Property(reader, tokenType); } }

        public Expression LineNumber { get { return Expression.Property(reader, lineNumber); } }

        public Expression LinePosition { get { return Expression.Property(reader, linePosition); } }

        public Expression Read() { return Expression.Call(reader, read); }

        public Expression Skip() { return Expression.Call(reader, skip); }

        public Expression Value { get { return Expression.Property(reader, value); } }
    }
}
