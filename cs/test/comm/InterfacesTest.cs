namespace UnitTest
{
    using Bond.Comm.Interfaces;
    using NUnit.Framework;

    [TestFixture]
    public class AttributesTests
    {
        [Test]
        public void SimpleAdder_DoesAddition()
        {
            Assert.AreEqual(12, SimpleAdder.Add(5, 7));
        }
    }
}
