namespace UnitTest
{
    using System;
    using Bond;
    using NUnit.Framework;

    /// <summary>
    /// Summary description for GuidConversionTests
    /// </summary>
    [TestFixture]
    public class GuidConversionTests
    {
        [Test]
        public void ImplicitConversion()
        {
            Assert.IsTrue(BitConverter.IsLittleEndian, "Only little endian is supported");
            var systemGuid = Guid.Parse("340FAA04-D19C-4B3D-A6F3-BA6581CCDCB8");
            var bondGuid = (GUID)systemGuid;

            // The Datas should be populated if it converted correctly
            Assert.AreEqual(bondGuid.Data1, 0x340FAA04);
            Assert.AreEqual(bondGuid.Data2, 0xD19C);
            Assert.AreEqual(bondGuid.Data3, 0x4B3D);
            Assert.AreEqual(bondGuid.Data4, 0xB8DCCC8165BAF3A6);

            // Converting back should result in an equal value
            Assert.AreEqual(systemGuid, (Guid)bondGuid);
        }
    }
}
