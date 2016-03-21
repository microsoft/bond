// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;

    /// <summary>
    /// Writes serialized payload
    /// </summary>
    public interface IProtocolWriter
    {
        /// <summary>
        /// Write protocol magic number and version
        /// </summary>
        void WriteVersion();

        /// <summary>
        /// Start writing a struct
        /// </summary>
        /// <param name="metadata">Schema metadata</param>
        void WriteStructBegin(Metadata metadata);

        /// <summary>
        /// Start writing a base struct
        /// </summary>
        /// <param name="metadata">Base schema metadata</param>
        void WriteBaseBegin(Metadata metadata);

        /// <summary>
        /// End writing a struct
        /// </summary>
        void WriteStructEnd();

        /// <summary>
        /// End writing a base struct
        /// </summary>
        void WriteBaseEnd();

        /// <summary>
        /// Start writing a field
        /// </summary>
        /// <param name="type">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
        void WriteFieldBegin(BondDataType type, ushort id, Metadata metadata);

        /// <summary>
        /// End writing a field
        /// </summary>
        void WriteFieldEnd();

        /// <summary>
        /// Indicate that field was omitted because it was set to its default value
        /// </summary>
        /// <param name="type">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
        void WriteFieldOmitted(BondDataType type, ushort id, Metadata metadata);

        /// <summary>
        /// Start writing a list or set container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="elementType">Type of the elements</param>
        void WriteContainerBegin(int count, BondDataType elementType);

        /// <summary>
        /// Start writing a map container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="keyType">Type of the keys</param>
        /// /// <param name="valueType">Type of the values</param>
        void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType);

        /// <summary>
        /// End writing a container
        /// </summary>
        void WriteContainerEnd();

        /// <summary>
        /// Write an Int8
        /// </summary>
        void WriteInt8(sbyte value);

        /// <summary>
        /// Write an Int16
        /// </summary>
        void WriteInt16(short value);

        /// <summary>
        /// Write an Int32
        /// </summary>
        void WriteInt32(int value);

        /// <summary>
        /// Write an Int64
        /// </summary>
        void WriteInt64(long value);

        /// <summary>
        /// Write an UInt8
        /// </summary>
        void WriteUInt8(byte value);

        /// <summary>
        /// Write an UInt16
        /// </summary>
        void WriteUInt16(ushort value);

        /// <summary>
        /// Write an UInt32
        /// </summary>
        void WriteUInt32(uint value);

        /// <summary>
        /// Write an UInt64
        /// </summary>
        void WriteUInt64(ulong value);

        /// <summary>
        /// Write a float
        /// </summary>
        void WriteFloat(float value);

        /// <summary>
        /// Write a double
        /// </summary>
        void WriteDouble(double value);

        /// <summary>
        /// Write array of bytes verbatim
        /// </summary>
        void WriteBytes(ArraySegment<byte> data);

        /// <summary>
        /// Write a bool
        /// </summary>
        void WriteBool(bool value);

        /// <summary>
        /// Write a UTF-8 string
        /// </summary>
        void WriteString(string value);

        /// <summary>
        /// Write a UTF-16 string
        /// </summary>
        void WriteWString(string value);
    }

    /// <summary>
    /// Provides interface to a writer to pre-run for protocols (like Compact Binary v2) which need two passes.
    /// </summary>
    public interface ITwoPassProtocolWriter: IProtocolWriter
    {
        /// <summary>
        /// Provide the first-pass writer, if one is needed for this writer.
        /// Otherwise, returns null.
        /// </summary>
        IProtocolWriter GetFirstPassWriter();
    }

    /// <summary>
    /// Writes elements used in text protocols
    /// </summary>
    public interface ITextProtocolWriter
    {
        /// <summary>
        /// Start writing a container item
        /// </summary>
        void WriteItemBegin();

        /// <summary>
        /// End writing a container item
        /// </summary>
        void WriteItemEnd();
    }
}
