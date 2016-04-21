// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// See CompactBinary.cs for a description of the CompactBinary protocol.

namespace Bond.Protocols
{
    using System;
    using System.Runtime.CompilerServices;
    using System.Text;
    using System.Collections.Generic;
    using Bond.IO;

    /// <summary>
    /// Length-calculator for Bond CompactBinary protocol V2
    /// </summary>
    [Reader(typeof(CompactBinaryReader<>))]
    public struct CompactBinaryCounter : IProtocolWriter
    {
        private class CounterStackFrame
        {
            public readonly LinkedListNode<UInt32> lengthSlot;
            public int currentLength;

            public CounterStackFrame(LinkedListNode<UInt32> slot)
            {
                lengthSlot = slot;
            }
        }

        readonly LinkedList<UInt32> lengths;
        readonly Stack<CounterStackFrame> counterStack;

        /// <summary>
        /// Create an instance of CompactBinaryCounter
        /// </summary>
        public CompactBinaryCounter(LinkedList<UInt32> lengthsOut)
        {
           lengths = lengthsOut;
           counterStack = new Stack<CounterStackFrame>();
        }

        private CounterStackFrame GetCurrentStackFrame()
        {
            return counterStack.Peek();
        }

        private void AddBytes(int count)
        {
            GetCurrentStackFrame().currentLength += count;
        }

        private void AddVarUInt16(ushort value)
        {
            AddBytes(IntegerHelper.GetVarUInt16Length(value));
        }

        private void AddVarUInt32(uint value)
        {
            AddBytes(IntegerHelper.GetVarUInt32Length(value));
        }

        private void AddVarUInt64(ulong value)
        {
            AddBytes(IntegerHelper.GetVarUInt64Length(value));
        }

        /// <summary>
        /// Write protocol magic number and version
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteVersion()
        {
        }

        #region Complex types
        /// <summary>
        /// Start writing a struct
        /// </summary>
        /// <param name="metadata">Schema metadata</param>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteStructBegin(Metadata metadata)
        {
            LinkedListNode<UInt32> frameNode = lengths.AddLast(0);
            counterStack.Push(new CounterStackFrame(frameNode));
        }

        /// <summary>
        /// End writing a struct
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteStructEnd()
        {
            CounterStackFrame frame = counterStack.Peek();
            uint structLength = (uint)frame.currentLength + 1; // +1 for the BT_STOP byte
            frame.lengthSlot.Value = structLength;
            counterStack.Pop();

            if (counterStack.Count > 0)
            {
                AddVarUInt32(structLength);
                AddBytes((int)structLength);
            }
        }

        /// <summary>
        /// Start writing a base struct
        /// </summary>
        /// <param name="metadata">Base schema metadata</param>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBaseBegin(Metadata metadata)
        { }

        /// <summary>
        /// End writing a base struct
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBaseEnd()
        {
            AddBytes(1);
        }

        /// <summary>
        /// Start writing a field
        /// </summary>
        /// <param name="type">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldBegin(BondDataType type, ushort id, Metadata metadata)
        {
            if (id <= 5)
            {
                AddBytes(1);
            }
            else if (id <= 0xFF)
            {
                AddBytes(2);
            }
            else
            {
                AddBytes(3);
            }
        }

        /// <summary>
        /// Indicate that field was omitted because it was set to its default value
        /// </summary>
        /// <param name="dataType">Type of the field</param>
        /// <param name="id">Identifier of the field</param>
        /// <param name="metadata">Metadata of the field</param>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldOmitted(BondDataType dataType, ushort id, Metadata metadata)
        { }


        /// <summary>
        /// End writing a field
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldEnd()
        { }

        /// <summary>
        /// Start writing a list or set container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="elementType">Type of the elements</param>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteContainerBegin(int count, BondDataType elementType)
        {
            if (count < 7)
            {
                AddBytes(1);
            }
            else
            {
                AddBytes(1);
                AddVarUInt32((uint)count);
            }
        }

        /// <summary>
        /// Start writing a map container
        /// </summary>
        /// <param name="count">Number of elements in the container</param>
        /// <param name="keyType">Type of the keys</param>
        /// <param name="valueType">Type of the values</param>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
        {
            AddBytes(2);
            AddVarUInt32((uint)count);
        }

        /// <summary>
        /// End writing a container
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteContainerEnd()
        { }

        /// <summary>
        /// Write array of bytes verbatim
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBytes(ArraySegment<byte> data)
        {
            AddBytes(data.Count);
        }

        #endregion

        #region Primitive types
        /// <summary>
        /// Write an UInt8
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt8(byte value)
        {
            AddBytes(1);
        }

        /// <summary>
        /// Write an UInt16
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt16(UInt16 value)
        {
            AddVarUInt16(value);
        }

        /// <summary>
        /// Write an UInt16
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt32(UInt32 value)
        {
            AddVarUInt32(value);
        }

        /// <summary>
        /// Write an UInt64
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt64(UInt64 value)
        {
            AddVarUInt64(value);
        }

        /// <summary>
        /// Write an Int8
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt8(SByte value)
        {
            AddBytes(1);
        }

        /// <summary>
        /// Write an Int16
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt16(Int16 value)
        {
            AddVarUInt16(IntegerHelper.EncodeZigzag16(value));
        }

        /// <summary>
        /// Write an Int32
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt32(Int32 value)
        {
            AddVarUInt32(IntegerHelper.EncodeZigzag32(value));
        }

        /// <summary>
        /// Write an Int64
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt64(Int64 value)
        {
            AddVarUInt64(IntegerHelper.EncodeZigzag64(value));
        }

        /// <summary>
        /// Write a float
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFloat(float value)
        {
            AddBytes(4);
        }

        /// <summary>
        /// Write a double
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteDouble(double value)
        {
            AddBytes(8);
        }

        /// <summary>
        /// Write a bool
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBool(bool value)
        {
            AddBytes(1);
        }

        /// <summary>
        /// Write a UTF-8 string
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteString(string value)
        {
            int size = Encoding.UTF8.GetByteCount(value);
            AddVarUInt32((uint)size);
            AddBytes(size);
        }

        /// <summary>
        /// Write a UTF-16 string
        /// </summary>
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteWString(string value)
        {
            AddVarUInt32((uint)value.Length);
            AddBytes(value.Length * 2);
        }
        #endregion
    }
}
