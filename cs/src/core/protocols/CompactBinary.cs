// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#region Compact Binary format
/*

                     .----------.--------------.   .----------.---------.
   struct (v1)       |  fields  | BT_STOP_BASE |...|  fields  | BT_STOP |
                     '----------'--------------'   '----------'---------'

                     .----------.----------.--------------.   .----------.---------.
   struct (v2)       |  length  |  fields  | BT_STOP_BASE |...|  fields  | BT_STOP |
                     '----------'----------'--------------'   '----------'---------'

   length             variable int encoded uint32 length of following fields, up to and
                      including BT_STOP but excluding length itself.

                     .----------.----------.   .----------.
   fields            |  field   |  field   |...|  field   |
                     '----------'----------'   '----------'

                     .----------.----------.
   field             | id+type  |  value   |
                     '----------'----------'

                                            .---.---.---.---.---.---.---.---.                       i - id bits
   id+type           0 <= id <= 5           | i | i | i | t | t | t | t | t |                       t - type bits
                                            '---'---'---'---'---'---'---'---'                       v - value bits
                                              2       0   4               0

                                            .---.---.---.---.---.---.---.---.---.   .---.
                     5 < id <= 0xff         | 1 | 1 | 0 | t | t | t | t | t | i |...| i |
                                            '---'---'---'---'---'---'---'---'---'   '---'
                                                          4               0   7       0

                                            .---.---.---.---.---.---.---.---.---.   .---.---.   .---.
                     0xff < id <= 0xffff    | 1 | 1 | 1 | t | t | t | t | t | i |...| i | i |...| i |
                                            '---'---'---'---'---'---'---'---'---'   '---'---'   '---'
                                                          4               0   7       0   15      8


                                            .---.---.---.---.---.---.---.---.
   value             bool                   |   |   |   |   |   |   |   | v |
                                            '---'---'---'---'---'---'---'---'
                                                                          0

                                            .---.---.---.---.---.---.---.---.
                     int8, uint8            | v | v | v | v | v | v | v | v |
                                            '---'---'---'---'---'---'---'---'
                                              7                           0

                                            .---.---.   .---.---.---.   .---.
                     uint16, uint32,        | 1 | v |...| v | 0 | v |...| v |  [...]
                     uint64                 '---'---'   '---'---'---'   '---'
                                                  6       0       13      7

                                            variable encoding, high bit of every byte
                                            indicates if there is another byte


                     int16, int32,          zig zag encoded to unsigned integer:
                     int64
                                             0 -> 0
                                            -1 -> 1
                                             1 -> 2
                                            -2 -> 3
                                            ...

                                            and then encoded as unsigned integer


                     float, double          little endian


                                            .-------.------------.
                     string, wstring        | count | characters |
                                            '-------'------------'

                           count            variable encoded uint32 count of 1-byte (for
                                            string) or 2-byte (for wstring) Unicode code
                                            units

                           characters       1-byte UTF-8 code units (for string) or 2-byte
                                            UTF-16LE code units (for wstring)


                                            .-------.-------.-------.
                     blob, list, set,       | type  | count | items |
                     vector, nullable       '-------'-------'-------'

                                            .---.---.---.---.---.---.---.---.
                           type (v1)        |   |   |   | t | t | t | t | t |
                                            '---'---'---'---'---'---'---'---'
                                                          4               0

                                            .---.---.---.---.---.---.---.---.
                           type (v2)        | c | c | c | t | t | t | t | t |
                                            '---'---'---'---'---'---'---'---'
                                              2       0   4               0

                                            if count of items is < 7, 'c' are bit of (count + 1),
                                            otherwise 'c' bits are 0.

                           count            variable encoded uint32 count of items
                                            omitted in v2 if 'c' bits within type byte are not 0

                           items            each item encoded according to its type


                                            .----------.------------.-------.-----.-------.
                     map                    | key type | value type | count | key | value |
                                            '----------'------------'-------'-----'-------'

                                            .---.---.---.---.---.---.---.---.
                            key type,       |   |   |   | t | t | t | t | t |
                            value type      '---'---'---'---'---'---'---'---'
                                                          4               0

                            count           variable encoded uint32 count of {key,mapped} pairs

                            key, mapped     each item encoded according to its type

*/
#endregion
namespace Bond.Protocols
{
    using System;
    using System.IO;
    using System.Runtime.CompilerServices;
    using System.Text;
    using System.Collections.Generic;
    using System.Diagnostics;
    using Bond.IO;

    /// <summary>
    /// Writer for the Compact Binary tagged protocol
    /// </summary>
    /// <typeparam name="O">Implementation of IOutputStream interface</typeparam>
    [Reader(typeof(CompactBinaryReader<>))]
    [FirstPassWriter(typeof(CompactBinaryCounter))]
    public struct CompactBinaryWriter<O> : ITwoPassProtocolWriter
        where O : IOutputStream
    {
        const ushort Magic = (ushort)ProtocolType.COMPACT_PROTOCOL;
        readonly O output;
        readonly ushort version;
        readonly CompactBinaryCounter? firstPassWriter;
        readonly LinkedList<uint> lengths;
        Stack<long> lengthCheck;

        /// <summary>
        /// Create an instance of CompactBinaryWriter
        /// </summary>
        /// <param name="output">Serialized payload output</param>
        /// <param name="version">Protocol version</param>
        public CompactBinaryWriter(O output, ushort version = 1)
        {
            this.output = output;
            this.version = version;
            if (version == 2)
            {
                lengths = new LinkedList<uint>();
                firstPassWriter = new CompactBinaryCounter(lengths);
            }
            else
            {
                lengths = null;
                firstPassWriter = null;
            }

            lengthCheck = null;
            InitLengthCheck();
        }

        public IProtocolWriter GetFirstPassWriter()
        {
            if (version == 2)
            {
                // Only return first pass if not in middle of second pass
                if (lengths.Count == 0)
                {
                    return firstPassWriter.Value;
                }
            }

            return null;
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

        #region Complex types
        /// <summary>
        /// Start writing a struct
        /// </summary>
        /// <param name="metadata">Schema metadata</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteStructBegin(Metadata metadata)
        {
            if (version == 2)
            {
                uint length = lengths.First.Value;
                lengths.RemoveFirst();

                output.WriteVarUInt32(length);
                PushLengthCheck(checked(output.Position + length));
            }
        }

        /// <summary>
        /// Start writing a base struct
        /// </summary>
        /// <param name="metadata">Base schema metadata</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBaseBegin(Metadata metadata)
        {}

        /// <summary>
        /// End writing a struct
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteStructEnd()
        {
            output.WriteUInt8((Byte)BondDataType.BT_STOP);
            PopLengthCheck(output.Position);
        }

        /// <summary>
        /// End writing a base struct
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBaseEnd()
        {
            output.WriteUInt8((Byte)BondDataType.BT_STOP_BASE);
        }

        /// <summary>
        /// Start writing a field
        /// </summary>
        /// <param name="type">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldBegin(BondDataType type, ushort id, Metadata metadata)
        {
            var fieldType = (uint)type;
            if (id <= 5)
            {
                output.WriteUInt8((byte)(fieldType | ((uint)id << 5)));
            }
            else if (id <= 0xFF)
            {
                output.WriteUInt16((ushort)(fieldType | (uint)id << 8 | (0x06 << 5)));
            }
            else
            {
                output.WriteUInt8((byte)(fieldType | (0x07 << 5)));
                output.WriteUInt16(id);
            }
        }


        /// <summary>
        /// Indicate that field was omitted because it was set to its default value
        /// </summary>
        /// <param name="dataType">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldOmitted(BondDataType dataType, ushort id, Metadata metadata)
        {}


        /// <summary>
        /// End writing a field
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldEnd()
        {}

        /// <summary>
        /// Start writing a list or set container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="elementType">Type of the elements</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerBegin(int count, BondDataType elementType)
        {
            if (2 == version && count < 7)
            {
                output.WriteUInt8((byte)((uint)elementType | (((uint)count + 1) << 5)));
            }
            else
            {
                output.WriteUInt8((byte)elementType);
                output.WriteVarUInt32((uint)count);
            }
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
            output.WriteUInt8((byte)keyType);
            output.WriteUInt8((byte)valueType);
            output.WriteVarUInt32((uint)count);
        }

        /// <summary>
        /// End writing a container
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerEnd()
        {}

        /// <summary>
        /// Write array of bytes verbatim
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBytes(ArraySegment<byte> data)
        {
            output.WriteBytes(data);
        }

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
            output.WriteVarUInt16(value);
        }

        /// <summary>
        /// Write an UInt16
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt32(UInt32 value)
        {
            output.WriteVarUInt32(value);
        }

        /// <summary>
        /// Write an UInt64
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt64(UInt64 value)
        {
            output.WriteVarUInt64(value);
        }

        /// <summary>
        /// Write an Int8
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt8(SByte value)
        {
            output.WriteUInt8((Byte)value);
        }

        /// <summary>
        /// Write an Int16
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt16(Int16 value)
        {
            output.WriteVarUInt16(IntegerHelper.EncodeZigzag16(value));
        }

        /// <summary>
        /// Write an Int32
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt32(Int32 value)
        {
            output.WriteVarUInt32(IntegerHelper.EncodeZigzag32(value));
        }

        /// <summary>
        /// Write an Int64
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt64(Int64 value)
        {
            output.WriteVarUInt64(IntegerHelper.EncodeZigzag64(value));
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

        /// <summary>
        /// Write a bool
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
                WriteUInt32(0);
            }
            else
            {
                var size = Encoding.UTF8.GetByteCount(value);
                WriteUInt32((UInt32)size);
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
                WriteUInt32(0);
            }
            else
            {
                int byteSize = checked(value.Length * 2);
                WriteUInt32((UInt32)value.Length);
                output.WriteString(Encoding.Unicode, value, byteSize);
            }
        }
        #endregion

        #region Length check
        [Conditional("DEBUG")]
        private void InitLengthCheck()
        {
            if (version == 2)
            {
                lengthCheck = new Stack<long>();
            }
        }

        [Conditional("DEBUG")]
        private void PushLengthCheck(long position)
        {
            lengthCheck.Push(position);
        }

        [Conditional("DEBUG")]
        private void PopLengthCheck(long position)
        {
            if (version == 2)
            {
                if (position != lengthCheck.Pop())
                {
                    Throw.EndOfStreamException();
                }
            }
        }
        #endregion
    }

    /// <summary>
    /// Reader for the Compact Binary tagged protocol
    /// </summary>
    /// <typeparam name="I">Implementation of IInputStream interface</typeparam>
    public struct CompactBinaryReader<I> : IClonableTaggedProtocolReader, ICloneable<CompactBinaryReader<I>>
        where I : IInputStream, ICloneable<I>
    {
        readonly I input;
        readonly ushort version;

        /// <summary>
        /// Create an instance of CompactBinaryReader
        /// </summary>
        /// <param name="input">Input payload</param>
        /// <param name="version">Protocol version</param>
        public CompactBinaryReader(I input, ushort version = 1)
        {
            this.input = input;
            this.version = version;
        }

        /// <summary>
        /// Clone the reader
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        CompactBinaryReader<I> ICloneable<CompactBinaryReader<I>>.Clone()
        {
            return new CompactBinaryReader<I>(input.Clone(), version);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        IClonableTaggedProtocolReader ICloneable<IClonableTaggedProtocolReader>.Clone()
        {
            return (this as ICloneable<CompactBinaryReader<I>>).Clone();
        }

        #region Complex types

        /// <summary>
        /// Start reading a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadStructBegin()
        {
            if (2 == version)
            {
                input.ReadVarUInt32();
            }
        }

        /// <summary>
        /// Start reading a base of a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadBaseBegin()
        { }

        /// <summary>
        /// End reading a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadStructEnd()
        { }

        /// <summary>
        /// End reading a base of a struct
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadBaseEnd()
        { }

        /// <summary>
        /// Start reading a field
        /// </summary>
        /// <param name="type">An out parameter set to the field type
        /// or BT_STOP/BT_STOP_BASE if there is no more fields in current struct/base</param>
        /// <param name="id">Out parameter set to the field identifier</param>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadFieldBegin(out BondDataType type, out ushort id)
        {
            uint raw = input.ReadUInt8();

            type = (BondDataType)(raw & 0x1f);
            raw >>= 5;

            if (raw < 6)
            {
                id = (ushort)raw;
            }
            else if (raw == 6)
            {
                id = input.ReadUInt8();
            }
            else
            {
                id = input.ReadUInt16();
            }
        }

        /// <summary>
        /// End reading a field
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadFieldEnd()
        { }

        /// <summary>
        /// Start reading a list or set container
        /// </summary>
        /// <param name="count">An out parameter set to number of items in the container</param>
        /// <param name="elementType">An out parameter set to type of container elements</param>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadContainerBegin(out int count, out BondDataType elementType)
        {
            var raw = input.ReadUInt8();
            elementType = (BondDataType)(raw & 0x1f);

            if (2 == version && (raw & (0x07 << 5)) != 0)
                count = (raw >> 5) - 1;
            else
                count = checked((int)input.ReadVarUInt32());
        }

        /// <summary>
        /// Start reading a map container
        /// </summary>
        /// <param name="count">An out parameter set to number of items in the container</param>
        /// <param name="keyType">An out parameter set to the type of map keys</param>
        /// <param name="valueType">An out parameter set to the type of map values</param>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadContainerBegin(out int count, out BondDataType keyType, out BondDataType valueType)
        {
            keyType = (BondDataType)input.ReadUInt8();
            valueType = (BondDataType)input.ReadUInt8();
            count = checked((int)input.ReadVarUInt32());
        }

        /// <summary>
        /// End reading a container
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void ReadContainerEnd()
        { }

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

        /// <summary>
        /// Read an UInt16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort ReadUInt16()
        {
            return input.ReadVarUInt16();
        }

        /// <summary>
        /// Read an UInt32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public uint ReadUInt32()
        {
            return input.ReadVarUInt32();
        }

        /// <summary>
        /// Read an UInt64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public UInt64 ReadUInt64()
        {
            return input.ReadVarUInt64();
        }

        /// <summary>
        /// Read an Int8
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public sbyte ReadInt8()
        {
            return (sbyte)input.ReadUInt8();
        }

        /// <summary>
        /// Read an Int16
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public short ReadInt16()
        {
            return IntegerHelper.DecodeZigzag16(input.ReadVarUInt16());
        }

        /// <summary>
        /// Read an Int32
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int ReadInt32()
        {
            return IntegerHelper.DecodeZigzag32(input.ReadVarUInt32());
        }

        /// <summary>
        /// Read an Int64
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Int64 ReadInt64()
        {
            return IntegerHelper.DecodeZigzag64(input.ReadVarUInt64());
        }

        /// <summary>
        /// Read a bool
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool ReadBool()
        {
            return input.ReadUInt8() != 0;
        }

        /// <summary>
        /// Read a float
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public float ReadFloat()
        {
            return input.ReadFloat();
        }

        /// <summary>
        /// Read a double
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double ReadDouble()
        {
            return input.ReadDouble();
        }

        /// <summary>
        /// Read a UTF-8 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public String ReadString()
        {
            var length = checked((int)input.ReadVarUInt32());
            return length == 0 ? string.Empty : input.ReadString(Encoding.UTF8, length);
        }

        /// <summary>
        /// Read a UTF-16 string
        /// </summary>
        /// <exception cref="EndOfStreamException"/>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public string ReadWString()
        {
            var length = checked((int)(input.ReadVarUInt32() * 2));
            return length == 0 ? string.Empty : input.ReadString(Encoding.Unicode, length);
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
        #endregion

        #region Skip
        /// <summary>
        /// Skip a value of specified type
        /// </summary>
        /// <param name="type">Type of the value to skip</param>
        /// <exception cref="EndOfStreamException"/>
        public void Skip(BondDataType type)
        {
            switch (type)
            {
                case (BondDataType.BT_BOOL):
                case (BondDataType.BT_UINT8):
                case (BondDataType.BT_INT8):
                    input.SkipBytes(sizeof(byte));
                    break;
                case (BondDataType.BT_UINT16):
                case (BondDataType.BT_INT16):
                    input.ReadVarUInt16();
                    break;
                case (BondDataType.BT_UINT32):
                case (BondDataType.BT_INT32):
                    input.ReadVarUInt32();
                    break;
                case (BondDataType.BT_FLOAT):
                    input.SkipBytes(sizeof(float));
                    break;
                case (BondDataType.BT_DOUBLE):
                    input.SkipBytes(sizeof(double));
                    break;
                case (BondDataType.BT_UINT64):
                case (BondDataType.BT_INT64):
                    input.ReadVarUInt64();
                    break;
                case (BondDataType.BT_STRING):
                    input.SkipBytes(checked((int)input.ReadVarUInt32()));
                    break;
                case (BondDataType.BT_WSTRING):
                    input.SkipBytes(checked((int)(input.ReadVarUInt32() *2)));
                    break;
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                    SkipContainer();
                    break;
                case BondDataType.BT_MAP:
                    SkipMap();
                    break;
                case BondDataType.BT_STRUCT:
                    SkipStruct();
                    break;
                default:
                    Throw.InvalidBondDataType(type);
                    break;
            }
        }


        void SkipContainer()
        {
            BondDataType elementType;
            int count;

            ReadContainerBegin(out count, out elementType);

            if (elementType == BondDataType.BT_UINT8 || elementType == BondDataType.BT_INT8)
            {
                input.SkipBytes(count);
            }
            else if (elementType == BondDataType.BT_FLOAT)
            {
                input.SkipBytes(checked(count * sizeof(float)));
            }
            else if (elementType == BondDataType.BT_DOUBLE)
            {
                input.SkipBytes(checked(count * sizeof(double)));
            }
            else
            {
                while (0 <= --count)
                {
                    Skip(elementType);
                }
            }
        }

        void SkipMap()
        {
            BondDataType keyType;
            BondDataType valueType;
            int count;

            ReadContainerBegin(out count, out keyType, out valueType);
            while (0 <= --count)
            {
                Skip(keyType);
                Skip(valueType);
            }
        }

        void SkipStruct()
        {
            if (2 == version)
            {
                input.SkipBytes(checked((int)input.ReadVarUInt32()));
            }
            else
            {
                while (true)
                {
                    BondDataType type;
                    ushort id;

                    ReadFieldBegin(out type, out id);

                    if (type == BondDataType.BT_STOP_BASE) continue;
                    if (type == BondDataType.BT_STOP) break;

                    Skip(type);
                }
            }
        }
        #endregion
    }
}
