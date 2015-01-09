// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System.IO;
    using System.Runtime.CompilerServices;

    internal static class Throw
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void InvalidBondDataType(BondDataType type)
        {
            throw new InvalidDataException(string.Format("Invalid BondDataType {0}", type));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void EndOfStreamException()
        {
            throw new EndOfStreamException("Unexpected end of stream reached.");
        }
    }
}
