// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System.Linq;
    using System;
    using System.Collections.Generic;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Protocols;
    using Bond.Internal.Reflection;

    internal class UntaggedReader<R>
    {
        static readonly MethodInfo unmarshalBonded = Reflection.MethodInfoOf(() => Unmarshal.From(default(ArraySegment<byte>)));
        static readonly MethodInfo fieldOmitted    = GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadFieldOmitted()));
        static readonly MethodInfo containerBegin =  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadContainerBegin()));
        static readonly MethodInfo containerEnd =    GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadContainerEnd()));
        static readonly MethodInfo readBytes =       GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadBytes(default(int))));
        static readonly MethodInfo skipBytes =       GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipBytes(default(int))));

        static readonly Dictionary<BondDataType, MethodInfo> read = new Dictionary<BondDataType, MethodInfo>
            {
                { BondDataType.BT_BOOL,    GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadBool())) },
                { BondDataType.BT_UINT8,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadUInt8())) },
                { BondDataType.BT_UINT16,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadUInt16())) },
                { BondDataType.BT_UINT32,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadUInt32())) },
                { BondDataType.BT_UINT64,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadUInt64())) },
                { BondDataType.BT_FLOAT,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadFloat())) },
                { BondDataType.BT_DOUBLE,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadDouble())) },
                { BondDataType.BT_STRING,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadString())) },
                { BondDataType.BT_INT8,    GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadInt8())) },
                { BondDataType.BT_INT16,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadInt16())) },
                { BondDataType.BT_INT32,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadInt32())) },
                { BondDataType.BT_INT64,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadInt64())) },
                { BondDataType.BT_WSTRING, GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.ReadWString())) }
            };

        static readonly Dictionary<BondDataType, MethodInfo> skip = new Dictionary<BondDataType, MethodInfo>
            {
                { BondDataType.BT_BOOL,    GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipBool())) },
                { BondDataType.BT_UINT8,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipUInt8())) },
                { BondDataType.BT_UINT16,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipUInt16())) },
                { BondDataType.BT_UINT32,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipUInt32())) },
                { BondDataType.BT_UINT64,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipUInt64())) },
                { BondDataType.BT_FLOAT,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipFloat())) },
                { BondDataType.BT_DOUBLE,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipDouble())) },
                { BondDataType.BT_STRING,  GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipString())) },
                { BondDataType.BT_INT8,    GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipInt8())) },
                { BondDataType.BT_INT16,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipInt16())) },
                { BondDataType.BT_INT32,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipInt32())) },
                { BondDataType.BT_INT64,   GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipInt64())) },
                { BondDataType.BT_WSTRING, GetMethod(Reflection.MethodInfoOf((IUntaggedProtocolReader reader) => reader.SkipWString())) }
            };

        static MethodInfo GetMethod(MethodInfo method)
        {
            // There is a method (sic!) to this madness. We need to get a method of type R, not method of the 
            // interface. Only this way the calls to methods of protocols that are implemented as a value types 
            // will be inlined by JIT. Inlining makes a big difference for performance.
            return typeof(R).FindMethod(method.Name, method.GetParameters().Select(p => p.ParameterType).ToArray());
        }

        readonly ParameterExpression reader = Expression.Parameter(typeof(R), "reader");

        public ParameterExpression Param { get { return reader; } }


        public Expression ReadFieldOmitted()
        {
            return Expression.Call(reader, fieldOmitted);
        }

        public Expression ReadContainerBegin()
        {
            return Expression.Call(reader, containerBegin);
        }

        public Expression ReadContainerEnd()
        {
            return Expression.Call(reader, containerEnd);
        }

        public Expression Read(BondDataType type)
        {
            return Expression.Call(reader, read[type]);
        }

        public Expression Skip(BondDataType type)
        {
            return Expression.Call(reader, skip[type]);
        }

        public Expression ReadBytes(Expression count)
        {
            return Expression.Call(reader, readBytes, count);
        }

        public Expression SkipBytes(Expression count)
        {
            return Expression.Call(reader, skipBytes, count);
        }

        public Expression ReadMarshaledBonded()
        {
            return Expression.Call(unmarshalBonded, 
                ReadBytes(Expression.Convert(Read(BondDataType.BT_UINT32), typeof(int))));
        }
    }
}
