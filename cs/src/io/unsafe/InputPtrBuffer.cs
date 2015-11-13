// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO.Unsafe
{
    using System;
    using System.Diagnostics;
    using System.IO;
    using System.Text;
    using Bond.Protocols;
    using RMarshal = System.Runtime.InteropServices.Marshal;

    /// <summary>
    /// Implements IInputStream on top of unmanaged memory buffer
    /// </summary>
    public unsafe sealed class InputPtrBuffer : IInputStream, ICloneable<InputPtrBuffer>
    {
        readonly int offset;
        internal byte* buffer;
        internal int end;
        internal int position;

        public long Length
        {
            get { return end - offset; }
        }

        public long Position
        {
            get { return position - offset; }
            set { position = offset + checked((int)value); }
        }

        public InputPtrBuffer(byte* data, int length)
            : this(data, 0, length)
        { }

        public InputPtrBuffer(byte* data, int offset, int length)
        {
            Debug.Assert(BitConverter.IsLittleEndian);

            buffer = data;
            this.offset = offset;
            end = offset + length;
            position = offset;
        }

        internal InputPtrBuffer(InputPtrBuffer that)
            : this(that.buffer, that.position, that.end - that.position)
        { }

        /// <summary>
        /// Create a clone of the current state of the buffer
        /// </summary>
        public InputPtrBuffer Clone()
        {
            return new InputPtrBuffer(this);
        }

        /// <summary>
        /// Skip forward specified number ot bytes
        /// </summary>
        /// <param name="count">Number of bytes to skip</param>
        /// <exception cref="EndOfStreamException"/>
        public void SkipBytes(int count)
        {
            position += count;
        }

        /// <summary>
        /// Read 8-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public byte ReadUInt8()
        {
            if (position >= end)
            {
                EndOfStream(1);
            }
            return buffer[position++];
        }

        /// <summary>
        /// Read little-endian encoded 16-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public ushort ReadUInt16()
        {
            if (position > end - sizeof(ushort))
            {
                EndOfStream(sizeof(ushort));
            }

            var result = *((ushort*)(buffer + position));
            position += sizeof(ushort);

            return result;
        }

        /// <summary>
        /// Read little-endian encoded 32-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public uint ReadUInt32()
        {
            if (position > end - sizeof(uint))
            {
                EndOfStream(sizeof(uint));
            }

            var result = *((uint*)(buffer + position));
            position += sizeof(uint);

            return result;
        }

        /// <summary>
        /// Read little-endian encoded 64-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public ulong ReadUInt64()
        {
            if (position > end - sizeof(ulong))
            {
                EndOfStream(sizeof(ulong));
            }

            var result = *((ulong*)(buffer + position));
            position += sizeof(ulong);
            return result;
        }

        /// <summary>
        /// Read little-endian encoded single precision ‎IEEE 754 float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public float ReadFloat()
        {
            if (position > end - sizeof(float))
            {
                EndOfStream(sizeof(float));
            }

            var result = *((float*)(buffer + position));
            position += sizeof(float);
            return result;
        }

        /// <summary>
        /// Read little-endian encoded double precision ‎IEEE 754 float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public double ReadDouble()
        {
            if (position > end - sizeof(double))
            {
                EndOfStream(sizeof(double));
            }
            var result = *((double*)(buffer + position));
            position += sizeof(double);
            return result;
        }

        /// <summary>
        /// Read an array of bytes verbatim
        /// </summary>
        /// <param name="count">Number of bytes to read</param>
        /// <exception cref="EndOfStreamException"/>
        public ArraySegment<byte> ReadBytes(int count)
        {
            if (position > end - count)
            {
                EndOfStream(count);
            }

            byte[] result = new byte[count];

            IntPtr pointer = (IntPtr)(buffer + position);
            RMarshal.Copy(pointer, result, 0, count);

            position += count;
            return new ArraySegment<byte>(result);
        }

        /// <summary>
        /// Read variable encoded 16-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public ushort ReadVarUInt16()
        {
            if (position >= end - IntegerHelper.MaxBytesVarInt16)
            {
                return (ushort)DecodeVarUInt64Checked();
            }
            return IntegerHelper.DecodeVarUInt16(buffer, ref position);
        }

        /// <summary>
        /// Read variable encoded 32-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public uint ReadVarUInt32()
        {
            if (position >= end - IntegerHelper.MaxBytesVarInt32)
            {
                return (uint)DecodeVarUInt64Checked();
            }
            return IntegerHelper.DecodeVarUInt32(buffer, ref position);
        }

        /// <summary>
        /// Read variable encoded 64-bit unsigned integer
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        public ulong ReadVarUInt64()
        {
            if (position >= end - IntegerHelper.MaxBytesVarInt64)
            {
                return DecodeVarUInt64Checked();
            }
            return IntegerHelper.DecodeVarUInt64(buffer, ref position);
        }

        /// <summary>
        /// Read UTF-8 or UTF-16 encoded string
        /// </summary>
        /// <param name="encoding">String encoding</param>
        /// <param name="size">Size of payload in bytes</param>
        public string ReadString(Encoding encoding, int size)
        {
            if (position > end - size)
            {
                EndOfStream(size);
            }

            sbyte* ptr = (sbyte*) (buffer + position);
            var result = new string(ptr, 0, size, encoding); // TODO: Check this

            position += size;
            return result;
        }

        private ulong DecodeVarUInt64Checked()
        {
            ulong raw = 0x80;
            ulong result = 0;
            var shift = 0;
            while (0x7Fu < raw && shift < 64)
            {
                if (position >= end)
                {
                    EndOfStream(1);
                }
                raw = buffer[position++];
                result |= (raw & 0x7Fu) << shift;
                shift += 7;
            }
            return result;
        }

        internal void EndOfStream(int count)
        {
            Throw.EndOfStreamException();
        }
    }
}