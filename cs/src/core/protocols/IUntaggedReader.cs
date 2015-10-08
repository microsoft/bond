// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;
    using System.IO;
    using Bond.IO;

    /// <summary>
    /// Reads from serialized payload encoded using a untagged protocol
    /// </summary>
    public interface IUntaggedProtocolReader
    {
        /// <summary>
        /// Check if the next field was omitted
        /// </summary>
        /// <returns>true iff the field was omitted</returns>
        bool ReadFieldOmitted();

        /// <summary>
        /// Start reading a container
        /// </summary>
        /// <returns>Number of element in the container</returns>
        int ReadContainerBegin();

        /// <summary>
        /// End reading a container
        /// </summary>
        void ReadContainerEnd();

        /// <summary> 
        /// Read an Int8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        sbyte ReadInt8();

        /// <summary> 
        /// Skip an Int8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipInt8();

        /// <summary>
        /// Read an Int16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        short ReadInt16();

        /// <summary>
        /// Skip an Int16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipInt16();

        /// <summary>
        /// Read an Int32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        int ReadInt32();

        /// <summary>
        /// Skip an Int32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipInt32();

        /// <summary>
        /// Read an Int64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        long ReadInt64();

        /// <summary>
        /// Skip an Int64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipInt64();

        /// <summary>
        /// Read a UInt8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        byte ReadUInt8();

        /// <summary>
        /// Skip an UInt8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipUInt8();

        /// <summary> 
        /// Read an UInt16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        ushort ReadUInt16();

        /// <summary>
        /// Skip an UInt16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipUInt16();

        /// <summary>
        /// Read an UInt32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        uint ReadUInt32();

        /// <summary>
        /// Skip an UInt32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipUInt32();

        /// <summary>
        /// Read an UInt64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        ulong ReadUInt64();

        /// <summary>
        /// Skip one UInt64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipUInt64();

        /// <summary>
        /// Read a float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        float ReadFloat();

        /// <summary>
        /// Skip an float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipFloat();

        /// <summary>
        /// Read a double
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        double ReadDouble();

        /// <summary>
        /// Skip a double
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipDouble();

        /// <summary>
        /// Read an array of bytes verbatim
        /// </summary>
        /// <param name="count">Number of bytes to read</param>
        ArraySegment<byte> ReadBytes(int count);

        /// <summary>
        /// Skip forward specified number of bytes
        /// </summary>
        /// <param name="count">Number of bytes to skip</param>
        /// <exception cref="EndOfStreamException"/>
        void SkipBytes(int count);

        /// <summary>
        /// Read a bool
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        bool ReadBool();

        /// <summary> 
        /// Skip a bool
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipBool();

        /// <summary>
        /// Read a UTF8 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        string ReadString();

        /// <summary>
        /// Skip a UTF8 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipString();

        /// <summary>
        /// Read a UTF16 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        string ReadWString();

        /// <summary>
        /// Skip a UTF16 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void SkipWString();
    }

    /// <summary>
    /// Extension of IUntaggedProtocolReader that can be used as R in Bonded&lt;T, R>
    /// </summary>
    public interface IClonableUntaggedProtocolReader
        : IUntaggedProtocolReader, ICloneable<IClonableUntaggedProtocolReader>
    { }
}
