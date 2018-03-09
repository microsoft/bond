// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using examples.inheritance;

    static class Program
    {
        static void Main()
        {
            var derivedObj = new Derived {str = "derived"};
            ((Base) derivedObj).str = "derived base";
            var derivedPayload = new CompactBinaryReader<InputBuffer>(
                new InputBuffer(Serialize(derivedObj)));

            // Deserialize Base from payload containing Derived
            Base obj = Deserialize<Base>.From(derivedPayload);
            ThrowIfFalse(Comparer.Equal<Base>(obj, derivedObj));
        }

        static ArraySegment<byte> Serialize<T>(T obj)
        {
            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            Bond.Serialize.To(writer, obj);
            return output.Data;
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
