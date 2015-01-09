namespace InternalTest
{
    using Bond.Expressions;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class RequiredFieldsTest
    {
        void TestBitmap(int count)
        {
            var b = new RequiredFields.Bitmap(count);
            Assert.IsTrue(b.IsAnySet);

            for (var i = 0; i < count; ++i)
            {
                Assert.AreEqual(i, b.FirstSet);
                b.Reset(i >> 6, 1L << (i % 64));
            }

            Assert.IsFalse(b.IsAnySet);
        }
        
        [TestMethod]
        public void BitmapTest()
        {
            for (var i = 1; i < 129; ++i)
            {
                TestBitmap(i);
            }
        }
    }
}
