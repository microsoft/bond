// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;
    using System.IO;
    using Bond.IO;

    /// <summary>
    /// Reads from serialize payload encoded using a tagged protocol
    /// </summary>
    public interface ITaggedProtocolReader
    {
        /// <summary>
        /// Start reading a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void ReadStructBegin();

        /// <summary>
        /// Start reading a base of a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void ReadBaseBegin();

        /// <summary>
        /// End reading a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void ReadStructEnd();

        /// <summary>
        /// End reading a base of a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void ReadBaseEnd();

        /// <summary>
        /// Start reading a field
        /// </summary>
        /// <param name="type">An out parameter set to the field type 
        /// or BT_STOP/BT_STOP_BASE if there is no more fields in current struct/base</param>
        /// <param name="id">An out parameter set to the field identifier</param>
        /// <exception cref="EndOfStreamException"/>
        void ReadFieldBegin(out BondDataType type, out ushort id);

        /// <summary>
        /// End reading a field
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void ReadFieldEnd();

        /// <summary>
        /// Start reading a list or set container
        /// </summary>
        /// <param name="count">An out parameter set to number of items in the container</param>
        /// <param name="elementType">An out parameter set to type of container elements</param>
        /// <exception cref="EndOfStreamException"/>
        void ReadContainerBegin(out int count, out BondDataType elementType);

        /// <summary>
        /// Start reading a map container
        /// </summary>
        /// <param name="count">An out parameter set to number of items in the container</param>
        /// <param name="keyType">An out parameter set to the type of map keys</param>
        /// <param name="valueType">An out parameter set to the type of map values</param>
        /// <exception cref="EndOfStreamException"/>
        void ReadContainerBegin(out int count, out BondDataType keyType, out BondDataType valueType);

        /// <summary>
        /// End reading a container
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        void ReadContainerEnd();

        /// <summary>
        /// Skip a value of specified type
        /// </summary>
        /// <param name="type">Type of the value to skip</param>
        /// <exception cref="EndOfStreamException"/>
        void Skip(BondDataType type);

        /// <summary>
        /// Read an Int8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        sbyte ReadInt8();

        /// <summary>
        /// Read an Int16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        short ReadInt16();

        /// <summary>
        /// Read an Int32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        int ReadInt32();

        /// <summary>
        /// Read an Int64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        long ReadInt64();

        /// <summary>
        /// Read an UInt8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        byte ReadUInt8();

        /// <summary>
        /// Read an UInt16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        ushort ReadUInt16();

        /// <summary>
        /// Read an UInt32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        uint ReadUInt32();

        /// <summary>
        /// Read an UInt64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        ulong ReadUInt64();

        /// <summary>
        /// Read a float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        float ReadFloat();

        /// <summary>
        /// Read a double
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        double ReadDouble();

        /// <summary>
        /// Read an array of bytes verbatim
        /// </summary>
        /// <param name="count">Number of bytes to read</param>
        /// <exception cref="EndOfStreamException"/>
        ArraySegment<byte> ReadBytes(int count);

        /// <summary>
        /// Read a bool
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        bool ReadBool();

        /// <summary>
        /// Read a UTF-8 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        string ReadString();

        /// <summary>
        /// Read a UTF-16 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        string ReadWString();
    }

    /// <summary>
    /// Extension of ITaggedProtocolReader that can be used as R in Bonded&lt;T, R>
    /// </summary>
    public interface IClonableTaggedProtocolReader
        : ITaggedProtocolReader, ICloneable<IClonableTaggedProtocolReader>
    { }
}
