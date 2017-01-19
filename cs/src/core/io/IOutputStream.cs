// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO
{
    using System;
    using System.Text;

    /// <summary>
    /// Writes primitive data types as binary values in a specific encoding
    /// </summary>
    public interface IOutputStream
    {
        /// <summary>
        /// Gets or sets the position within the stream
        /// </summary>
        /// <exception cref="NotSupportedException"/>
        long Position { get; set; }

        /// <summary>
        /// Write 8-bit unsigned integer
        /// </summary>
        void WriteUInt8(byte value);

        /// <summary>
        /// Write little-endian encoded 16-bit unsigned integer
        /// </summary>
        void WriteUInt16(ushort value);

        /// <summary>
        /// Write little-endian encoded 32-bit unsigned integer
        /// </summary>
        void WriteUInt32(uint value);

        /// <summary>
        /// Write little-endian encoded 64-bit unsigned integer
        /// </summary>
        void WriteUInt64(ulong value);

        /// <summary>
        /// Write little-endian encoded single precision IEEE 754 float
        /// </summary>
        void WriteFloat(float value);

        /// <summary>
        /// Write little-endian encoded double precision IEEE 754 float
        /// </summary>
        void WriteDouble(double value);

        /// <summary>
        /// Write an array of bytes verbatim
        /// </summary>
        /// <param name="data">Array segment specifying bytes to write</param>
        void WriteBytes(ArraySegment<byte> data);

        /// <summary>
        /// Write variable encoded 16-bit unsigned integer
        /// </summary>
        void WriteVarUInt16(ushort value);

        /// <summary>
        /// Write variable encoded 32-bit unsigned integer
        /// </summary>
        void WriteVarUInt32(uint value);

        /// <summary>
        /// Write variable encoded 64-bit unsigned integer
        /// </summary>
        void WriteVarUInt64(ulong value);

        /// <summary>
        /// Write UTF-8 or UTF-16 encoded string
        /// </summary>
        /// <param name="encoding">String encoding</param>
        /// <param name="value">String value</param>
        /// <param name="size">Size in bytes of encoded string</param>
        void WriteString(Encoding encoding, string value, int size);
    }
}
