namespace UnitTest
{
    using System.Threading;
    using Bond;
    using NUnit.Framework;

    [TestFixture]
    public class RuntimeBondedTests
    {
        [Test]
        public void Deserialize_ReturnsSameObjectWhenObjectIsProvided()
        {
            object expected = new object();

            var target = new RuntimeBonded<object>(expected);

            object actual = target.Deserialize();

            Assert.AreSame(expected, actual);
        }

        [Test]
        public void Deserialize_CallsValueDeserializerFuncOnceWhenCalledOnce()
        {
            object expected = new object();
            int valueDeserializerFuncCallCount = 0;

            var target = new RuntimeBonded<object>(() =>
            {
                Interlocked.Increment(ref valueDeserializerFuncCallCount);

                return expected;
            });

            target.Deserialize();

            Assert.AreEqual(1, valueDeserializerFuncCallCount);
        }

        [Test]
        public void Deserialize_CallsValueDeserializerFuncTwiceWhenCalledTwice()
        {
            object expected = new object();
            int valueDeserializerFuncCallCount = 0;

            var target = new RuntimeBonded<object>(() =>
            {
                Interlocked.Increment(ref valueDeserializerFuncCallCount);

                return expected;
            });

            target.Deserialize();
            target.Deserialize();

            Assert.AreEqual(2, valueDeserializerFuncCallCount);
        }

        [Test]
        public void Deserialize_ReturnsSameObjectReturnedByValueDeserializerFunc()
        {
            object expected = new object();

            var target = new RuntimeBonded<object>(() =>
            {
                return expected;
            });

            var actual = target.Deserialize();

            Assert.AreSame(expected, actual);
        }
    }
}
