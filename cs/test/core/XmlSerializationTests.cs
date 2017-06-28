namespace UnitTest
{
    using System;
    using System.Text;
    using System.Xml;
    using Bond;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    class XmlSerializationTests
    {
        [Test]
        public void XmlSerialization_NullNonNullableString_Throws()
        {
            var xmlString = new StringBuilder();
            var xmlWriter = new SimpleXmlWriter(XmlWriter.Create(xmlString));

            var nullString = new BasicTypes {_str = null};
            Assert.Throws<NullReferenceException>(() => Serialize.To(xmlWriter, nullString));

            var nullWString = new BasicTypes {_wstr = null};
            Assert.Throws<NullReferenceException>(() => Serialize.To(xmlWriter, nullWString));
        }
    }
}
