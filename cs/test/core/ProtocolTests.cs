namespace UnitTest
{
    using System;
    using Bond;
    using Bond.Protocols;
    using NUnit.Framework;

    [Reader(typeof(CompactBinaryReader<>))]
    public class NotImplementedWriter : IProtocolWriter
    {
        public void WriteVersion()
        {
            throw new NotImplementedException();
        }

        public virtual void WriteStructBegin(Metadata metadata)
        {
            throw new NotImplementedException();
        }

        public void WriteBaseBegin(Metadata metadata)
        {
            throw new NotImplementedException();
        }

        public void WriteStructEnd()
        {
            throw new NotImplementedException();
        }

        public void WriteBaseEnd()
        {
            throw new NotImplementedException();
        }

        public void WriteFieldBegin(BondDataType type, ushort id, Metadata metadata)
        {
            throw new NotImplementedException();
        }

        public void WriteFieldEnd()
        {
            throw new NotImplementedException();
        }

        public void WriteFieldOmitted(BondDataType type, ushort id, Metadata metadata)
        {
            throw new NotImplementedException();
        }

        public void WriteContainerBegin(int count, BondDataType elementType)
        {
            throw new NotImplementedException();
        }

        public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
        {
            throw new NotImplementedException();
        }

        public void WriteContainerEnd()
        {
            throw new NotImplementedException();
        }

        public void WriteInt8(sbyte value)
        {
            throw new NotImplementedException();
        }

        public void WriteInt16(short value)
        {
            throw new NotImplementedException();
        }

        public void WriteInt32(int value)
        {
            throw new NotImplementedException();
        }

        public void WriteInt64(long value)
        {
            throw new NotImplementedException();
        }

        public void WriteUInt8(byte value)
        {
            throw new NotImplementedException();
        }

        public void WriteUInt16(ushort value)
        {
            throw new NotImplementedException();
        }

        public void WriteUInt32(uint value)
        {
            throw new NotImplementedException();
        }

        public void WriteUInt64(ulong value)
        {
            throw new NotImplementedException();
        }

        public void WriteFloat(float value)
        {
            throw new NotImplementedException();
        }

        public void WriteDouble(double value)
        {
            throw new NotImplementedException();
        }

        public void WriteBytes(ArraySegment<byte> data)
        {
            throw new NotImplementedException();
        }

        public void WriteBool(bool value)
        {
            throw new NotImplementedException();
        }

        public void WriteString(string value)
        {
            throw new NotImplementedException();
        }

        public void WriteWString(string value)
        {
            throw new NotImplementedException();
        }
    }

    [Reader(typeof(CompactBinaryReader<>))]
    public class DerivedWriter : NotImplementedWriter
    {
        public override void WriteStructBegin(Metadata metadata)
        {
            throw new NotImplementedException();
        }
    }

    [TestFixture]
    public class ProtocolTests
    {
        [Test]
        public void DerivedWriter()
        {
            new Serializer<DerivedWriter>(typeof (BasicTypes));
        }
    }
}
