// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System.Linq.Expressions;
    using System.Reflection;

    internal static class DataExpression
    {
        // Similar to Expression.PropertyOrField but considers only members declared
        // in the type, ignoring inherited members.
        public static MemberExpression PropertyOrField(Expression expression, string name)
        {
            var property = expression.Type.GetTypeInfo().GetDeclaredProperty(name);
            return (property != null) ?
                Expression.Property(expression, property) :
                Expression.Field(expression, expression.Type.GetTypeInfo().GetDeclaredField(name));
        }
    }
}
