namespace UnitTest
{
    using System.Linq;
    using System.Reflection;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using Bond;

    [TestClass]
    public class AttributesTests
    {
        static T GetAttribute<T>(MemberInfo type)
            where T : class
        {
            return type.GetCustomAttributes(typeof(T), false).FirstOrDefault() as T;
        }
        
        [TestMethod]
        public void Attributes_Default_Value_Is_Public()
        {
            // ensure that DefaultAttribute's value is public
            Assert.AreEqual("a", GetAttribute<DefaultAttribute>(typeof(Foo).GetProperty("A")).Value);
        }

        [TestMethod]
        public void Attributes_Attribute_Members_Are_Public()
        {
            // ensure that TypeAttribute's value is public
            var attributeAttribute = new AttributeAttribute("Foo", "Bar");
            Assert.AreEqual("Foo", attributeAttribute.Name);
            Assert.AreEqual("Bar", attributeAttribute.Value);
        }

        private class Foo
        {
            [Default("a")]
            public string A { get; set; }

            [Bond.Type(typeof(nullable<string>))]
            public string B { get; set; }
        }
    }
}
