// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System.Linq.Expressions;

    /// <summary>
    /// Helpers for creating loop expression trees
    /// </summary>
    internal static class ControlExpression
    {
        public static Expression While(Expression whileCondition, Expression body)
        {
            return While(whileCondition, body, Expression.Label("end"));
        }

        public static Expression While(Expression whileCondition, Expression body, LabelTarget breakLabel)
        {
            return Expression.Loop(
                PrunedExpression.IfThenElse(
                    whileCondition,
                    body,
                    Expression.Break(breakLabel)),
                breakLabel);
        }

        public static Expression DoWhile(Expression body, Expression condition)
        {
            return DoWhile(body, condition, Expression.Label("end"));
        }

        public static Expression DoWhile(Expression body, Expression condition, LabelTarget breakLabel)
        {
            return Expression.Loop(
                Expression.Block(
                    body,
                    Expression.IfThen(Expression.Not(condition), Expression.Break(breakLabel))),
                breakLabel);
        }
    }
}
