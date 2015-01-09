// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Linq.Expressions;

    public interface ISerializerGenerator<R, W>
    {
        IEnumerable<Expression<Action<R, W>>> Generate(IParser parser);
    }
}
