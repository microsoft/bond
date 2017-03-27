// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Grpc
{
    using System;

    using Bond.IO.Unsafe;
    using Bond.Protocols;

    public static class Marshaller<T>
    {
        static Marshaller()
        {
            Instance = new global::Grpc.Core.Marshaller<IMessage<T>>(ToByteArray, FromByteArray);
        }

        public static global::Grpc.Core.Marshaller<IMessage<T>> Instance { get; }

        private static byte[] ToByteArray(IMessage<T> input)
        {
            if (input == null)
            {
                return null;
            }

            var output = new OutputBuffer(128);
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            input.Payload.Serialize(writer);

            var arraySegment = output.Data;

            if (arraySegment.Offset == 0 && arraySegment.Count == arraySegment.Array.Length)
            {
                // perfect match, so we can just return the backing buffer
                return arraySegment.Array;
            }

            // size or offset doesn't line up, so we need to make a copy
            var bytes = new byte[arraySegment.Count];
            Buffer.BlockCopy(
                src: arraySegment.Array,
                srcOffset: arraySegment.Offset,
                dst: bytes,
                dstOffset: 0,
                count: arraySegment.Count);
            return bytes;
        }

        private static IMessage<T> FromByteArray(byte[] arr)
        {
            if (arr == null)
            {
                return null;
            }

            var input = new InputBuffer(arr);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            var bonded = new Bonded<T, CompactBinaryReader<InputBuffer>>(reader);

            return Message.From<T>(bonded);
        }
    }
}
