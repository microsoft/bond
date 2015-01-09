// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;

    // Extension methods for Blob called by generated code to allow custom type mappings for blob aliases.
    internal static class Blob
    {
        public static bool CompareData(this ArraySegment<byte> lhs, ArraySegment<byte> rhs)
        {
            if (lhs.Count == 0 && rhs.Count == 0)
                return true;

            if ((lhs.Array == null) != (rhs.Array == null))
                return false;

            if (lhs.Array == null)
                return true;

            if (lhs.Count != rhs.Count)
                return false;

            // ReSharper disable once PossibleNullReferenceException
            // rhs.Array is checked above
            for (var i = 0; i < lhs.Count; ++i)
                if (lhs.Array[lhs.Offset + i] != rhs.Array[rhs.Offset + i])
                    return false;

            return true;
        }
    }
}
