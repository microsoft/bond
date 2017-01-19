// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO.Unsafe
{
    using System;
    using System.IO;

    /// <summary>
    /// Implements IInputStream on top of memory buffer
    /// </summary>
    public class InputBuffer : Safe.InputBuffer, ICloneable<InputBuffer>
    {
        public InputBuffer(byte[] data)
            : base(data, 0, data.Length)
        {}

        public InputBuffer(byte[] data, int length)
            : base(data, 0, length)
        {}

        public InputBuffer(ArraySegment<byte> seg)
            : base(seg.Array, seg.Offset, seg.Count)
        {}

        public InputBuffer(byte[] data, int offset, int length)
            : base(data, offset, length)
        {}

        internal InputBuffer(InputBuffer that)
            : base(that)
        {}

        /// <summary>
        /// Create a clone of the current state of the buffer
        /// </summary>
        public new InputBuffer Clone()
        {
            return new InputBuffer(this);
        }

        /// <summary>
        /// Read little-endian encoded 16-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public override ushort ReadUInt16()
        {
            if (position > end - sizeof(ushort))
            {
                EndOfStream(sizeof(ushort));
            }
            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    position += sizeof(ushort);
                    return *((ushort*)p);
                }
            }
        }

        /// <summary>
        /// Read little-endian encoded 32-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public override uint ReadUInt32()
        {
            if (position > end - sizeof(uint))
            {
                EndOfStream(sizeof(uint));
            }
            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    position += sizeof(uint);
                    return *((uint*)p);
                }
            }
        }

        /// <summary>
        /// Read little-endian encoded 64-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public override ulong ReadUInt64()
        {
            if (position > end - sizeof(ulong))
            {
                EndOfStream(sizeof(ulong));
            }
            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    position += sizeof(ulong);
                    return *((ulong*)p);
                }
            }
        }

        /// <summary>
        /// Read little-endian encoded single precision IEEE 754 float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public override float ReadFloat()
        {
            if (position > end - sizeof(float))
            {
                EndOfStream(sizeof(float));
            }
            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    position += sizeof(float);
                    return *((float*)p);
                }
            }
        }

        /// <summary>
        /// Read little-endian encoded double precision IEEE 754 float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public override double ReadDouble()
        {
            if (position > end - sizeof(double))
            {
                EndOfStream(sizeof(double));
            }
            unsafe
            {
                fixed (void* p = &buffer[position])
                {
                    position += sizeof(double);
                    return *((double*)p);
                }
            }
        }
    }
}
