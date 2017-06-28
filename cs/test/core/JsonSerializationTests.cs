namespace UnitTest
{
    using System;
    using System.IO;
    using Bond;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    class JsonSerializationTests
    {
        [Test]
        public void JsonSerialization_NullNonNullableString_Throws()
        {
            var ser = new Serializer<SimpleJsonWriter>(typeof(BasicTypes));
            var stream = new StringWriter();
            var jw = new SimpleJsonWriter(stream);

            var nullString = new BasicTypes {_str = null};
            Assert.Throws<NullReferenceException>(() => ser.Serialize(nullString, jw));

            var nullWString = new BasicTypes {_wstr = null};
            Assert.Throws<NullReferenceException>(() => ser.Serialize(nullWString, jw));
        }
    }
}
