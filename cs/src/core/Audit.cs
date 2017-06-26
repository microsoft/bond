// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Runtime.CompilerServices;

    internal static class Audit
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        static void FailArgNull(string paramName)
        {
            throw new ArgumentNullException(paramName);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void FailArgRule(string message)
        {
            throw new ArgumentException(message);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void ArgNotNull(object value, string paramName)
        {
            if (value == null)
            {
                FailArgNull(paramName);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void ArgRule(bool invariant, string message)
        {
            if (!invariant)
            {
                FailArgRule(message);
            }
        }
    }
}
