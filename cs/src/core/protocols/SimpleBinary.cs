// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#region Simple Binary format
/*
                     .-------------.----------------.
   struct            | base fields | derived fields |
                     '-------------'----------------'

                     .----------.----------.   .----------.
   fields            |  field   |  field   |...|  field   |
                     '----------'----------'   '----------'

                     .----------.
   field             |  value   |
                     '----------'

                                           .---.---.---.---.---.---.---.---.
   value            bool                   |   |   |   |   |   |   |   | v |
                                           '---'---'---'---'---'---'---'---'
                                                                          0

                    all integral types are written binary, native size, uncompressed, little endian

                    float, double          little endian


                                            .-------.------------.
                     string, wstring        | count | characters |
                                            '-------'------------'

                           count            uint32 count of 1-byte (for string)
                                            or 2-byte (for wstring) Unicode code
                                            units (variable encoded in v2)

                           characters       1-byte UTF-8 code units (for string) or 2-byte
                                            UTF-16LE code units (for wstring)


                                           .-------. .-------.
                    blob, list, set,       | count | | items |...
                    vector, nullable       '-------' '-------'

                           count            uint32 count of items (variable encoded in v2)

                           items            each item encoded according to its type

                                           .-------. .-----.--------.
                    map                    | count | | key | mapped |...
                                           '-------' '-----'--------'

                            count           uint32 count of {key,mapped} pairs (variable encoded in v2)

                            key, mapped     each item encoded according to its type

                                           .-------. .-----------.
                    bonded                 | count | | marshaled |
                                           '-------' '-----------'
                            count           uint32 count of bytes (always fixed-width, even in v2)

                            marshaled       a marshaled payload

*/
#endregion

namespace Bond.Protocols
{
    using System;
    using System.IO;
    using System.Runtime.CompilerServices;
    using System.Text;
    using Bond.IO;

    [Reader(typeof(SimpleBinaryReader<>))]
    public struct SimpleBinaryWriter<O> : IProtocolWriter
        where O : IOutputStream
    {
        const ushort Magic = (ushort)ProtocolType.SIMPLE_PROTOCOL;
        readonly O output;
        readonly ushort version;

        /// <summary> Construct a new SimpleBinaryWriter
        /// </summary>
        /// <param name="output">Serialize payload output</param>
        /// <param name="version">Protocol version</param>
        public SimpleBinaryWriter(O output, ushort version = 1)
        {
            this.output = output;
            this.version = version;
        }

        /// <summary>
        /// Write protocol magic number and version
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteVersion()
        {
            output.WriteUInt16(Magic);
            output.WriteUInt16(version);
        }

        #region Complex Types

        #region Unused in tagged protocol
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldBegin(BondDataType type, ushort id, Metadata metadata) { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldEnd() { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteStructBegin(Metadata metadata) { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBaseBegin(Metadata metadata) { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteStructEnd() { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBaseEnd() { }

        #endregion

        /// <summary>
        /// Indicate that field was omitted because it was set to its default value
        /// </summary>
        /// <param name="dataType">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldOmitted(BondDataType dataType, ushort id, Metadata metadata)
        {
            // Simple doesn't support omitting fields so instead we write the default value
            Audit.ArgRule(!metadata.default_value.nothing, "Field set to nothing can't be serialized.");

            switch (dataType)
            {
                case BondDataType.BT_BOOL:
                    WriteBool(0 != metadata.default_value.uint_value);
                    break;
                case BondDataType.BT_UINT8:
                    WriteUInt8((byte)metadata.default_value.uint_value);
                    break;
                case BondDataType.BT_UINT16:
                    WriteUInt16((UInt16)metadata.default_value.uint_value);
                    break;
                case BondDataType.BT_UINT32:
                    WriteUInt32((UInt32)metadata.default_value.uint_value);
                    break;
                case BondDataType.BT_UINT64:
                    WriteUInt64(metadata.default_value.uint_value);
                    break;
                case BondDataType.BT_FLOAT:
                    WriteFloat((float)metadata.default_value.double_value);
                    break;
                case BondDataType.BT_DOUBLE:
                    WriteDouble(metadata.default_value.double_value);
                    break;
                case BondDataType.BT_STRING:
                    WriteString(metadata.default_value.string_value);
                    break;
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                case BondDataType.BT_MAP:
                    WriteContainerBegin(0, dataType);
                    break;
                case BondDataType.BT_INT8:
                    WriteInt8((sbyte)metadata.default_value.int_value);
                    break;
                case BondDataType.BT_INT16:
                    WriteInt16((Int16)metadata.default_value.int_value);
                    break;
                case BondDataType.BT_INT32:
                    WriteInt32((Int32)metadata.default_value.int_value);
                    break;
                case BondDataType.BT_INT64:
                    WriteInt64(metadata.default_value.int_value);
                    break;
                case BondDataType.BT_WSTRING:
                    WriteWString(metadata.default_value.wstring_value);
                    break;
                default:
                    Throw.InvalidBondDataType(dataType);
                    break;
            }
        }

        /// <summary>
        /// Start writing a list or set container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="elementType">Type of the elements</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerBegin(int count, BondDataType elementType)
        {
            WriteLength(count);
        }

        /// <summary>
        /// Start writing a map container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="keyType">Type of the keys</param>
        /// /// <param name="valueType">Type of the values</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
        {
            WriteLength(count);
        }

        /// <summary>
        /// End writing a container
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerEnd() { }

        #endregion

        #region Primitive types
        /// <summary>
        /// Write an UInt8
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt8(Byte value)
        {
            output.WriteUInt8(value);
        }

        /// <summary>
        /// Write an UInt16
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt16(UInt16 value)
        {
            output.WriteUInt16(value);
        }

        /// <summary>
        /// Write an UInt32
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt32(UInt32 value)
        {
            output.WriteUInt32(value);
        }

        /// <summary>
        /// Write an UInt64
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt64(UInt64 value)
        {
            output.WriteUInt64(value);
        }

        /// <summary>
        /// Write array of bytes verbatim
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBytes(ArraySegment<byte> data)
        {
            output.WriteBytes(data);
        }

        /// <summary>
        /// Write an Int8
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt8(SByte value)
        {
            output.WriteUInt8(unchecked((Byte)value));
        }

        /// <summary>
        /// Write an Int16
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt16(Int16 value)
        {
            output.WriteUInt16(unchecked((UInt16) value));
        }

        /// <summary>
        /// Write an Int32
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt32(Int32 value)
        {
            output.WriteUInt32(unchecked((UInt32)value));
        }

        /// <summary>
        /// Write an Int64
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt64(Int64 value)
        {
            output.WriteUInt64(unchecked((UInt64)value));
        }

        /// <summary>
        /// Write a float
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFloat(float value)
        {
            output.WriteFloat(value);
        }

        /// <summary>
        /// Write a double
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteDouble(double value)
        {
            output.WriteDouble(value);
        }

        /// <summary> write bool, extending the stream if necessary and possible
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBool(bool value)
        {
            output.WriteUInt8((byte)(value ? 1 : 0));
        }

        /// <summary>
        /// Write a UTF-8 string
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteString(string value)
        {
            if (value.Length == 0)
            {
                WriteLength(0);
            }
            else
            {
                var size = Encoding.UTF8.GetByteCount(value);
                WriteLength(size);
                output.WriteString(Encoding.UTF8, value, size);
            }
        }

        /// <summary>
        /// Write a UTF-16 string
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteWString(string value)
        {
            if (value.Length == 0)
            {
                WriteLength(0);
            }
            else
            {
                WriteLength(value.Length);
                output.WriteString(Encoding.Unicode, value, checked(value.Length * 2));
            }
        }
        #endregion

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        void WriteLength(int value)
        {
            if (version == 2)
                output.WriteVarUInt32((uint)value);
            else
                output.WriteUInt32((uint)value);
        }

    }

    public struct SimpleBinaryReader<I> : IClonableUntaggedProtocolReader, ICloneable<SimpleBinaryReader<I>>
        where I : IInputStream, ICloneable<I>
    {
        readonly I input;
        readonly ushort version;

        public SimpleBinaryReader(I reader, ushort version = 1)
        {
            input = reader;
            this.version = version;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        SimpleBinaryReader<I> ICloneable<SimpleBinaryReader<I>>.Clone()
        {
            return new SimpleBinaryReader<I>(input.Clone(), version);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        IClonableUntaggedProtocolReader ICloneable<IClonableUntaggedProtocolReader>.Clone()
        {
            return (this as ICloneable<SimpleBinaryReader<I>>).Clone();
        }

        #region Complex types

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool ReadFieldOmitted()
        {
            return false;
        }

        /// <summary>
        /// Start reading a list or set container
        /// </summary>
        /// <returns>Number of elements</returns>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int ReadContainerBegin()
        {
            return ReadLength();
        }

        /// <summary>
        /// End reading a container
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadContainerEnd()
        {}

        #endregion

        #region Primitive types

        /// <summary>
        /// Read an UInt8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte ReadUInt8()
        {
            return input.ReadUInt8();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipUInt8()
        {
            input.SkipBytes(1);
        }

        /// <summary>
        /// Read an UInt16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort ReadUInt16()
        {
            return input.ReadUInt16();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipUInt16()
        {
            input.SkipBytes(2);
        }

        /// <summary>
        /// Read an UInt32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public uint ReadUInt32()
        {
            return input.ReadUInt32();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipUInt32()
        {
            input.SkipBytes(4);
        }

        /// <summary>
        /// Read an UInt64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public UInt64 ReadUInt64()
        {
            return input.ReadUInt64();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipUInt64()
        {
            input.SkipBytes(8);
        }

        /// <summary>
        /// Read an Int8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public sbyte ReadInt8()
        {
            return unchecked((sbyte)input.ReadUInt8());
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipInt8()
        {
            input.SkipBytes(1);
        }

        /// <summary>
        /// Read an Int16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public short ReadInt16()
        {
            return unchecked((short)input.ReadUInt16());
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipInt16()
        {
            input.SkipBytes(2);
        }

        /// <summary>
        /// Read an Int32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int ReadInt32()
        {
            return unchecked((int)input.ReadUInt32());
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipInt32()
        {
            input.SkipBytes(4);
        }

        /// <summary>
        /// Read an Int64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Int64 ReadInt64()
        {
            return unchecked((Int64)input.ReadUInt64());
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipInt64()
        {
            input.SkipBytes(8);
        }

        /// <summary>
        /// Read an bool
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool ReadBool()
        {
            return input.ReadUInt8() != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipBool()
        {
            input.SkipBytes(1);
        }

        /// <summary>
        /// Read an float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public float ReadFloat()
        {
            return input.ReadFloat();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipFloat()
        {
            input.SkipBytes(4);
        }

        /// <summary>
        /// Read an double
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double ReadDouble()
        {
            return input.ReadDouble();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipDouble()
        {
            input.SkipBytes(8);
        }

        /// <summary>
        /// Read a UTF-8 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public String ReadString()
        {
            var length = ReadLength();
            return length == 0 ? string.Empty : input.ReadString(Encoding.UTF8, length);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipString()
        {
            input.SkipBytes(ReadLength());
        }

        /// <summary>
        /// Read a UTF-16 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public string ReadWString()
        {
            var length = ReadLength();
            return length == 0 ? string.Empty : input.ReadString(Encoding.Unicode, checked(length * 2));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipWString()
        {
            input.SkipBytes(checked(ReadLength() * 2));
        }

        /// <summary>
        /// Read an array of bytes verbatim
        /// </summary>
        /// <param name="count">Number of bytes to read</param>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ArraySegment<byte> ReadBytes(int count)
        {
            return input.ReadBytes(count);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SkipBytes(int count)
        {
            input.SkipBytes(count);
        }

        #endregion

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        int ReadLength()
        {
            return (version == 2) ? checked((int)input.ReadVarUInt32()) : checked((int)input.ReadUInt32());
        }
    }
}
