// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace CodegenTest.DefaultIgnoresOutput
{
    public static class UsesCodegen
    {
        public static SimpleType Make()
        {
            return new SimpleType
            {
                _int32 = 42,
            };
        }
    }
}
