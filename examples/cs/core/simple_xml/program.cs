namespace Examples
{
    using System;
    using System.IO;
    using System.Text;
    using System.Xml;

    using Bond;
    using Bond.Protocols;

    static class Program
    {
        static void Main()
        {
            var config = new Config
            {
                Variant = "Simple",
                Enabled = true,
                Urls = { "http://example.com", "http://www.example.com" }
            };

            var xmlString = new StringBuilder();
            var xmlWriter = new SimpleXmlWriter(XmlWriter.Create(xmlString, new XmlWriterSettings
            {
                OmitXmlDeclaration = true,
                Indent = true
            }));

            Serialize.To(xmlWriter, config);
            xmlWriter.Flush();
            Console.WriteLine(xmlString);

            const string configString =
@"<Config>
  <Urls>
    <Item>http://example.com</Item>
  </Urls>
  <Enabled>false</Enabled>
  <Variant>Complex</Variant>
</Config>";

            var reader = new SimpleXmlReader(new StringReader(configString));
            config = Deserialize<Config>.From(reader);
            ThrowIfFalse(config.Enabled == false);
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
