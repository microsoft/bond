namespace UnitTest
{
    using System;
    using System.IO;
    using System.Xml;
    using System.Xml.Schema;
    using Bond;
    using Bond.Protocols;

    using NUnit.Framework;

    [TestFixture]
    public class XmlParsingTests
    {
        static readonly XmlReaderSettings xmlReaderSettings =
            new XmlReaderSettings
            {
                IgnoreProcessingInstructions = true,
#if !NETCOREAPP1_0
                ValidationType = ValidationType.None,
                ValidationFlags = XmlSchemaValidationFlags.None,
#endif
#if SUPPORTS_XMLRESOLVER
                XmlResolver = null,
#endif
            };

        [Test]
        public void XmlParsing_EmptyStruct()
        {
            const string xml = @"<BasicTypes/>";
            var target = ParseXml<BasicTypes>(xml);
            Assert.IsNotNull(target);
        }

        [Test]
        public void XmlParsing_BasicModel()
        {
            const string xml = @"
<BasicTypes>
    <_str>Hello</_str>
    <_bool>true</_bool>
    <_double>13.2</_double>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void XmlParsing_HandlesXmlDeclaration()
        {
            const string xml = @"<?xml version=""1.0"" encoding=""UTF-16""?>
<BasicTypes>
    <_str>Hello</_str>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual("Hello", target._str);
        }

        [Test]
        public void XmlParsing_IgnoresComments()
        {
            const string xml = @"
<BasicTypes>
    <!--comment-->
    <_str>Hello</_str>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual("Hello", target._str);
        }

        [Test]
        public void XmlParsing_UnexpectedNodeType_Throws()
        {
            // We're currently using CDATA here to exercise the unexpected node code path. If we
            // use CDATA in the future for things like blobs, this test will need to be changed.
            const string xml = @"
<BasicTypes>
   <![CDATA[here is an unexpected CDATA node]]>
    <_str>Hello</_str>
</BasicTypes>";

            Assert.That(() => ParseXml<BasicTypes>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Unexpected node type: CDATA"));
        }

        [Test]
        public void XmlParsing_InvalidScalar_Throws()
        {
            const string xml = @"
<BasicTypes>
    <_double>13.2<foo/></_double>
</BasicTypes>";

            Assert.That(() => ParseXml<BasicTypes>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Parsing error"));
        }

        [Test]
        public void XmlParsing_BasicModelWithUnknownElement()
        {
            const string xml = @"
<BasicTypes>
    <_str>Hello</_str>
    <foo></foo>
    <_bool>true</_bool>
    <_double>13.2</_double>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void XmlParsing_BasicModelWithUnknownEmptyElement()
        {
            const string xml = @"
<BasicTypes>
    <_str>Hello</_str>
    <foo/>
    <_bool>true</_bool>
    <_double>13.2</_double>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void XmlParsing_MultilineString()
        {
            const string xml = @"
<BasicTypes>
    <_str>Hello
World</_str>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual("Hello\nWorld", target._str);
        }

        [Test]
        public void XmlParsing_WhitespaceString()
        {
            const string xml = @"
<BasicTypes>
    <_str> </_str>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.AreEqual(" ", target._str);
        }

        [Test]
        public void XmlParsing_EmptyStringField()
        {
            const string xml = @"
<BasicTypes>
    <_str/>
    <_wstr></_wstr>
    <_bool>true</_bool>
    <_double>13.2</_double>
<unexpectedElement><withNesting/></unexpectedElement>
</BasicTypes>";

            var target = ParseXml<BasicTypes>(xml);

            Assert.IsEmpty(target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void XmlParsing_EmptyFieldScalarField_Throws()
        {
            const string xml = @"<BasicTypes><_bool/></BasicTypes>";

            Assert.Throws<FormatException>(() => ParseXml<BasicTypes>(xml));
        }

        [Test]
        public void XmlParsing_Nested()
        {
            const string xml = @"
<Nested>
  <basic>
    <BasicTypes>
      <_str>Hello</_str>
      <_bool>true</_bool>
     <_double>13.2</_double>
    </BasicTypes>
  </basic>
  <nested>
    <Nested1>
      <basic1><BasicTypes><_int32>17</_int32></BasicTypes></basic1>
      <basic2><BasicTypes><_int16>-101</_int16></BasicTypes></basic2>
    </Nested1>
  </nested>
</Nested>";

            var target = ParseXml<Nested>(xml);

            Assert.AreEqual("Hello", target.basic._str);
            Assert.IsTrue(target.basic._bool);
            Assert.AreEqual(13.2, target.basic._double);
            Assert.AreEqual(17, target.nested.basic1._int32);
            Assert.AreEqual(-101, target.nested.basic2._int16);
        }

        [Test]
        public void XmlParsing_Derived()
        {
            const string xml = @"
<Derived xmlns:n=""urn:UnitTest.Nested"" xmlns:d=""urn:UnitTest.Derived"">
  <d:derived>foo</d:derived>
  <n:basic>
    <BasicTypes>
      <_str>Hello</_str>
      <_bool>true</_bool>
     <_double>13.2</_double>
    </BasicTypes>
  </n:basic>
  <d:nested>
    <Nested1>
      <basic1><BasicTypes><_int32>-111</_int32></BasicTypes></basic1>
      <basic2><BasicTypes><_int16>113</_int16></BasicTypes></basic2>
    </Nested1>
  </d:nested>
  <n:nested>
    <Nested1>
      <basic1><BasicTypes><_int32>17</_int32></BasicTypes></basic1>
      <basic2><BasicTypes><_int16>-101</_int16></BasicTypes></basic2>
    </Nested1>
  </n:nested>
</Derived>";

            var target = ParseXml<Derived>(xml);

            Assert.AreEqual("Hello", target.basic._str);
            Assert.IsTrue(target.basic._bool);
            Assert.AreEqual(13.2, target.basic._double);
            Assert.AreEqual(17, (target as Nested).nested.basic1._int32);
            Assert.AreEqual(-101, (target as Nested).nested.basic2._int16);
            Assert.AreEqual(-111, target.nested.basic1._int32);
            Assert.AreEqual(113, target.nested.basic2._int16);
            Assert.AreEqual("foo", target.derived);
        }

        [Test]
        public void XmlParsing_EmptyNested()
        {
            const string xml = @"
<Nested>
  <basic>
    <BasicTypes/>
  </basic>
  <nested>
    <Nested1>
      <basic1><BasicTypes><_int32>17</_int32></BasicTypes></basic1>
      <basic2><BasicTypes><_int16>-101</_int16></BasicTypes></basic2>
    </Nested1>
  </nested>
</Nested>";

            var target = ParseXml<Nested>(xml);

            Assert.AreEqual(17, target.nested.basic1._int32);
            Assert.AreEqual(-101, target.nested.basic2._int16);
        }

        [Test]
        public void XmlParsing_MultipleEmptyNested()
        {
            const string xml = @"
<Nested>
  <nested>
    <Nested1>
      <basic1><BasicTypes/></basic1>
      <basic2><BasicTypes/></basic2>
    </Nested1>
  </nested>
  <basic>
    <BasicTypes>
      <_str>Test</_str>
    </BasicTypes>
  </basic>
</Nested>";

            var target = ParseXml<Nested>(xml);

            Assert.IsNotNull(target);
            Assert.IsNotNull(target.nested.basic1);
            Assert.IsNotNull(target.nested.basic2);
            Assert.AreEqual("Test", target.basic._str);
        }

        [Test]
        public void XmlParsing_StructPropertyWithoutContent_Throws()
        {
            const string xml = @"
<Nested>
  <basic/>
</Nested>";

            Assert.That(() => ParseXml<Nested>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Parsing error"));
        }

        [Test]
        public void XmlParsing_MissingRequiredEmptyRoot_Throws()
        {
            const string xml = "<Required/>";

            Assert.That(() => ParseXml<Required>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.Required.x missing"));
        }

        [Test]
        public void XmlParsing_MissingRequiredScalarField_Throws()
        {
            const string xml = @"
<Required>
    <y>
        <BasicTypes/>
    </y>
</Required>";

            Assert.That(() => ParseXml<Required>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.Required.x missing"));
        }

        [Test]
        public void XmlParsing_MissingRequiredStructField_Throws()
        {
            const string xml = @"
<Required>
    <x>
        5
    </x>
</Required>";

            Assert.That(() => ParseXml<Required>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.Required.y missing"));
        }

        [Test]
        public void XmlParsing_MissingRequiredInBase_Throws()
        {
            const string xml = @"
<RequiredInBase xmlns:b=""urn:UnitTest.Required""  xmlns:d=""urn:UnitTest.RequiredInBase"">
    <d:y><BasicTypes/></d:y>
    <d:x>5</d:x>
</RequiredInBase>";

            Assert.That(() => ParseXml<RequiredInBase>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.Required.x missing"));
        }

        [Test]
        public void XmlParsing_MissingRequiredInDerived_Throws()
        {
            const string xml = @"
<RequiredInDerived xmlns:b=""urn:UnitTest.Optional""  xmlns:d=""urn:UnitTest.RequiredInDerived"">
    <b:y><BasicTypes/></b:y>
    <b:x>5</b:x>
</RequiredInDerived>";

            Assert.That(() => ParseXml<RequiredInDerived>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.RequiredInDerived.x missing"));
        }

        [Test]
        public void XmlParsing_MissingRequiredInBasePresentInDerived_Throws()
        {
            const string xml = @"
<RequiredInBaseAndDerived xmlns:b=""urn:UnitTest.Required""  xmlns:d=""urn:UnitTest.RequiredInBaseAndDerived"">
    <d:y><BasicTypes/></d:y>
    <d:x>5</d:x>
</RequiredInBaseAndDerived>";

            Assert.That(() => ParseXml<RequiredInBaseAndDerived>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.Required.x missing"));
        }

        [Test]
        public void XmlParsing_MissingRequiredInDerivedPresentInBase_Throws()
        {
            const string xml = @"
<RequiredInBaseAndDerived xmlns:b=""urn:UnitTest.Required""  xmlns:d=""urn:UnitTest.RequiredInBaseAndDerived"">
    <b:y><BasicTypes/></b:y>
    <b:x>5</b:x>
</RequiredInBaseAndDerived>";

            Assert.That(() => ParseXml<RequiredInBaseAndDerived>(xml),
                Throws.TypeOf<InvalidDataException>()
                    .With.Message.Contains("Required field UnitTest.RequiredInBaseAndDerived.x missing"));
        }

        [Test]
        public void XmlParsing_PresentRequiredInBaseInDerived()
        {
            const string xml = @"
<RequiredInBaseAndDerived xmlns:b=""urn:UnitTest.Required""  xmlns:d=""urn:UnitTest.RequiredInBaseAndDerived"">
    <d:y><BasicTypes/></d:y>
    <b:y><BasicTypes/></b:y>
    <b:x>5</b:x>
    <d:x>5</d:x>
</RequiredInBaseAndDerived>";

            var target = ParseXml<RequiredInBaseAndDerived>(xml);

            Assert.AreEqual(5u, target.x);
        }

        [Test]
        public void XmlParsing_Containers()
        {
            const string xml = @"
<SimpleContainers>
  <strings>
    <Item>foo</Item>
    <Item>bar</Item>
  </strings>
  <basics>
    <Item>
        <BasicTypes>
          <_str>first</_str>
        </BasicTypes>
    </Item>
    <Item>
      <BasicTypes>
        <_str>second</_str>
      </BasicTypes>
    </Item>
    <Item>
      <BasicTypes>
        <_str>third</_str>
      </BasicTypes>
    </Item>
  </basics>
  <numbers>
    <Item>1</Item>
    <Item>one</Item>
    <Item>5</Item>
    <Item>five</Item>
  </numbers>
</SimpleContainers>";
            var target = ParseXml<SimpleContainers>(xml);

            Assert.AreEqual(2, target.strings.Count);
            Assert.AreEqual("foo", target.strings[0]);
            Assert.AreEqual("bar", target.strings[1]);
            Assert.AreEqual(3, target.basics.Count);
            Assert.AreEqual("first", target.basics[0]._str);
            Assert.AreEqual("second", target.basics[1]._str);
            Assert.AreEqual("third", target.basics[2]._str);
            Assert.AreEqual(2, target.numbers.Count);
            Assert.AreEqual("one", target.numbers[1]);
            Assert.AreEqual("five", target.numbers[5]);
        }

        [Test]
        public void XmlParsing_EmptyContainers()
        {
            const string xml = @"
<SimpleContainers>
  <strings/>
  <basics>
    <Item>
      <BasicTypes>
        <_str>first</_str>
      </BasicTypes>
    </Item>
    <Item>
      <BasicTypes>
        <_str>second</_str>
      </BasicTypes>
    </Item>
    <Item>
      <BasicTypes>
        <_str>third</_str>
      </BasicTypes>
    </Item>
  </basics>
</SimpleContainers>";
            var target = ParseXml<SimpleContainers>(xml);

            Assert.AreEqual(0, target.strings.Count);
            Assert.AreEqual(3, target.basics.Count);
            Assert.AreEqual("first", target.basics[0]._str);
            Assert.AreEqual("second", target.basics[1]._str);
            Assert.AreEqual("third", target.basics[2]._str);
        }

        [Test]
        public void XmlParsing_NestedContainers()
        {
            const string xml = @"
<NestedContainers>
  <vvb>
    <Item>
      <Item>7</Item>
      <Item>13</Item>
    </Item>
    <Item>
      <Item>5</Item>
    </Item>
    <Item>
      <Item>9</Item>
    </Item>
  </vvb>
</NestedContainers>";

            var target = ParseXml<NestedContainers>(xml);

            Assert.AreEqual(3, target.vvb.Count);
            Assert.AreEqual(2, target.vvb[0].Count);
            Assert.AreEqual(1, target.vvb[1].Count);
            Assert.AreEqual(1, target.vvb[2].Count);
            Assert.AreEqual((uint)7, target.vvb[0][0]);
            Assert.AreEqual((uint)13, target.vvb[0][1]);
            Assert.AreEqual((uint)5, target.vvb[1][0]);
            Assert.AreEqual((uint)9, target.vvb[2][0]);
        }

        [Test]
        public void XmlParsing_EmptyNestedContainers()
        {
            const string xml = @"
<NestedContainers>
  <vvb>
    <Item>
      <Item>7</Item>
      <Item>13</Item>
    </Item>
    <Item></Item>
    <Item>
      <Item>9</Item>
    </Item>
    <Item/>
  </vvb>
</NestedContainers>";

            var target = ParseXml<NestedContainers>(xml);

            Assert.AreEqual(4, target.vvb.Count);
            Assert.AreEqual(2, target.vvb[0].Count);
            Assert.AreEqual(0, target.vvb[1].Count);
            Assert.AreEqual(1, target.vvb[2].Count);
            Assert.AreEqual(0, target.vvb[3].Count);
            Assert.AreEqual((uint)7, target.vvb[0][0]);
            Assert.AreEqual((uint)13, target.vvb[0][1]);
            Assert.AreEqual((uint)9, target.vvb[2][0]);
        }

        [Test]
        public void XmlParsing_EmptyStructsInContainer()
        {
            const string xml = @"
<SimpleContainers>
  <basics>
    <Item><BasicTypes/></Item>
    <Item><BasicTypes/></Item>
    <Item>
      <BasicTypes>
        <_str>Something</_str>
      </BasicTypes>
    </Item>
    <Item><BasicTypes/></Item>
  </basics>
  <strings><Item>Hello</Item></strings>
</SimpleContainers>";

            var target = ParseXml<SimpleContainers>(xml);

            Assert.AreEqual(4, target.basics.Count);
            Assert.AreEqual("Something", target.basics[2]._str);
            Assert.AreEqual(1, target.strings.Count);
            Assert.AreEqual("Hello", target.strings[0]);
        }

        [Test]
        public void XmlParsing_Recursive()
        {
            const string xml = @"
<Tree>
    <root>
        <Item>
            <TreeNode>
                <left>
                    <Item>
                        <TreeNode>
                            <left/>
                            <right/>
                            <value>
                                <BasicTypes>
                                    <_str>Hello</_str>
                                </BasicTypes>
                            </value>
                        </TreeNode>
                    </Item>
                </left>
                <right/>
                <value>
                    <BasicTypes>
                        <_double>13.2</_double>
                    </BasicTypes>
                </value>
            </TreeNode>
        </Item>
    </root>
</Tree>";
            var target = ParseXml<Tree>(xml);

            Assert.AreEqual("Hello", target.root.left.value._str);
            Assert.AreEqual(13.2, target.root.value._double);
            Assert.AreEqual(null, target.root.left.left);
            Assert.AreEqual(null, target.root.left.right);
            Assert.AreEqual(null, target.root.right);
        }

        static T ParseXml<T>(string xml) where T : new()
        {
            var reader = new SimpleXmlReader(XmlReader.Create(new StringReader(xml), xmlReaderSettings));

            return Deserialize<T>.From(reader);
        }
    }
}
