// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO.Unsafe
{
    using Bond.Protocols;
    using System;
    using System.Diagnostics;
    using System.Runtime.InteropServices;
    using System.Text;

    /// <summary>
    /// Implements IOutputStream on top of unmanaged memory buffer
    /// </summary>
    public sealed unsafe class OutputPtrBuffer : IOutputStream
    {
        internal byte* data;
        internal int position;
        internal int length;

        /// <summary>
        /// Gets data inside the buffer
        /// </summary>
        public byte* Data
        {
            get { return data; }
        }

        /// <summary>
        /// Gets or sets the current position within the buffer
        /// </summary>
        public long Position
        {
            get { return position; }
            set { position = checked((int)value); }
        }

        public OutputPtrBuffer(byte* buffer, int length)
        {
            Debug.Assert(BitConverter.IsLittleEndian);

            this.data = buffer;
            this.length = length;

            position = 0;
        }

        #region IOutputStream

        /// <summary>
        /// Write 8-bit unsigned integer
        /// </summary>
        public void WriteUInt8(byte value)
        {
            if (position >= length)
            {
                EndOfStream(sizeof(byte));
            }

            data[position++] = value;
        }

        /// <summary>
        /// Write little-endian encoded 16-bit unsigned integer
        /// </summary>
        public void WriteUInt16(ushort value)
        {
            if (position + sizeof(ushort) > length)
            {
                EndOfStream(sizeof(ushort));
            }

            var i = position;
            var b = data;
            b[i++] = (byte)value;
            b[i++] = (byte)(value >> 8);
            position = i;
        }

        /// <summary>
        /// Write little-endian encoded 32-bit unsigned integer
        /// </summary>
        public void WriteUInt32(uint value)
        {
            if (position + sizeof(uint) > length)
            {
                EndOfStream(sizeof(uint));
            }

            var i = position;
            var b = data;
            b[i++] = (byte)value;
            b[i++] = (byte)(value >> 8);
            b[i++] = (byte)(value >> 16);
            b[i++] = (byte)(value >> 24);
            position = i;
        }

        /// <summary>
        /// Write little-endian encoded 64-bit unsigned integer
        /// </summary>
        public void WriteUInt64(ulong value)
        {
            if (position + sizeof(ulong) > length)
            {
                EndOfStream(sizeof(ulong));
            }

            var i = position;
            var b = data;
            b[i++] = (byte)value;
            b[i++] = (byte)(value >> 8);
            b[i++] = (byte)(value >> 16);
            b[i++] = (byte)(value >> 24);
            b[i++] = (byte)(value >> 32);
            b[i++] = (byte)(value >> 40);
            b[i++] = (byte)(value >> 48);
            b[i++] = (byte)(value >> 56);
            position = i;
        }

        /// <summary>
        /// Write little-endian encoded single precision ‎IEEE 754 float
        /// </summary>
        public void WriteFloat(float value)
        {
            WriteUInt32(new FloatLayout { value = value }.bytes);
        }

        /// <summary>
        /// Write little-endian encoded double precision ‎IEEE 754 float
        /// </summary>
        public void WriteDouble(double value)
        {
            WriteUInt64(new DoubleLayout { value = value }.bytes);
        }

        /// <summary>
        /// Write an array of bytes verbatim
        /// </summary>
        /// <param name="bytes">Array segment specifying bytes to write</param>
        public void WriteBytes(ArraySegment<byte> bytes)
        {
            var newOffset = position + bytes.Count;
            if (newOffset > length)
            {
                EndOfStream(bytes.Count);
            }

            // We don't use a BlockCopyMin because this implementation is even faster than looping after 3 bytes.
            // In fact it is even faster than Buffer.BlockCopy and System.Buffer (.Net 4.6) by a wide margin in both Legacy and RyuJIT.
            fixed ( byte* bytesPtr = bytes.Array )
            {
                byte* ptr = bytesPtr + bytes.Offset;
                MemoryHelper.Copy(data + position, ptr, bytes.Count);
            }
         
            position = newOffset;
        }

        /// <summary>
        /// Write variable encoded 16-bit unsigned integer
        /// </summary>
        public void WriteVarUInt16(ushort value)
        {
            if (position + IntegerHelper.MaxBytesVarInt16 > length)
            {
                EndOfStream(IntegerHelper.MaxBytesVarInt16);
            }
            position = IntegerHelper.EncodeVarUInt16(data, value, position);
        }

        /// <summary>
        /// Write variable encoded 32-bit unsigned integer
        /// </summary>
        public void WriteVarUInt32(uint value)
        {
            if (position + IntegerHelper.MaxBytesVarInt32 > length)
            {
                EndOfStream(IntegerHelper.MaxBytesVarInt32);
            }
            position = IntegerHelper.EncodeVarUInt32(data, value, position);
        }

        /// <summary>
        /// Write variable encoded 64-bit unsigned integer
        /// </summary>
        public void WriteVarUInt64(ulong value)
        {
            if (position + IntegerHelper.MaxBytesVarInt64 > length)
            {
                EndOfStream(IntegerHelper.MaxBytesVarInt64);
            }
            position = IntegerHelper.EncodeVarUInt64(data, value, position);
        }


        /// <summary>
        /// Write UTF-8 or UTF-16 encoded string
        /// </summary>
        /// <param name="encoding">String encoding</param>
        /// <param name="value">String value</param>
        /// <param name="size">Size in bytes of encoded string</param>
        public void WriteString(Encoding encoding, string value, int size)
        {
            if (position + size > length)
            {
                EndOfStream(size);
            }

            fixed (char* valuePtr = value)
            {
                position += encoding.GetBytes(valuePtr, value.Length, data + position, length - position);
            }
        }

        #endregion

        internal void EndOfStream(int count)
        {
            Throw.EndOfStreamException();
        }

        #region layouts

        [StructLayout(LayoutKind.Explicit)]
        struct DoubleLayout
        {
            [FieldOffset(0)]
            public readonly ulong bytes;

            [FieldOffset(0)]
            public double value;
        }

        [StructLayout(LayoutKind.Explicit)]
        struct FloatLayout
        {
            [FieldOffset(0)]
            public readonly uint bytes;

            [FieldOffset(0)]
            public float value;
        }

        #endregion
    }
}