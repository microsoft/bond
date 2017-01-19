// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO.Unsafe
{
    /// <summary>
    /// Implements IOutputStream on top of memory buffer
    /// </summary>
    public class OutputBuffer : Safe.OutputBuffer
    {
        public OutputBuffer()
        {}

        public OutputBuffer(int length)
            : base(length)
        {}

        public OutputBuffer(byte[] data)
            : base(data)
        {}

        /// <summary>
        /// Write little-endian encoded 32-bit unsigned integer
        /// </summary>
        public override void WriteUInt32(uint value)
        {
            if (position + sizeof(uint) > length)
                Grow(sizeof(uint));

            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    *((uint*)p) = value;
                }
            }
            position += sizeof(uint);
        }

        /// <summary>
        /// Write little-endian encoded 64-bit unsigned integer
        /// </summary>
        public override void WriteUInt64(ulong value)
        {
            if (position + sizeof(ulong) > length)
                Grow(sizeof(ulong));

            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    *((ulong*)p) = value;
                }
            }
            position += sizeof(ulong);
        }

        /// <summary>
        /// Write little-endian encoded single precision IEEE 754 float
        /// </summary>
        public override void WriteFloat(float value)
        {
            if (position + sizeof(float) > length)
                Grow(sizeof(float));

            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    *((float*)p) = value;
                }
            }
            position += sizeof(float);
        }

        /// <summary>
        /// Write little-endian encoded double precision IEEE 754 float
        /// </summary>
        public override void WriteDouble(double value)
        {
            if (position + sizeof(double) > length)
                Grow(sizeof(double));

            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    *((double*)p) = value;
                }
            }
            position += sizeof(double);
        }
    }
}
