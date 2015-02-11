namespace UnitTest
{
    using NUnit.Framework;

    [TestFixture]
    public class MetaInitializationTests
    {
        [Test]
        public void MetaInitialization_Polymorphic_Initialized()
        {
            var polymorphic = new WithPolymorphic();
            Assert.AreEqual("UnitTest.WithPolymorphic", polymorphic._bond_meta);
            Assert.AreEqual("a", polymorphic.a);

            var derived = new DerivedPolymorphic();
            Assert.AreEqual("UnitTest.DerivedPolymorphic", derived._bond_meta);
            Assert.AreEqual("a", derived.a);
            Assert.AreEqual("b", derived.b);
        }

        [Test]
        public void MetaInitialization_Meta_Initialized()
        {
            var withMeta = new WithMeta();
            Assert.AreEqual("UnitTest.WithMeta", withMeta.theFullName);
            Assert.AreEqual("WithMeta", withMeta.theName);
            Assert.AreEqual("a", withMeta.a);

            var derived = new DerivedWithMeta();
            Assert.AreEqual("UnitTest.DerivedWithMeta", derived.theFullName);
            Assert.AreEqual("DerivedWithMeta", derived.theName);
            Assert.AreEqual("a", derived.a);
            Assert.AreEqual("b", derived.b);

            var conflicting = new WithConflictingMeta();
            Assert.AreEqual("Foo", conflicting.name);
            Assert.AreEqual("Bar", conflicting.fullName);
            Assert.AreEqual("UnitTest.WithConflictingMeta", conflicting.meta);

            // TODO: add support for names of generic schemas
        }
    }
}
