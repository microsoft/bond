namespace Examples
{
    using System;
    using System.Text;
    using System.Xml;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                Widgets =
                {
                    new Widget { Name = "Konfabulator", Number = 3.14 }
                }
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var xmlString = new StringBuilder();
            var xmlWriter = new SimpleXmlWriter(XmlWriter.Create(xmlString, new XmlWriterSettings
            {
                OmitXmlDeclaration = true,
                Indent = true
            }));
            Transcode<Example>.FromTo(reader, xmlWriter);
            xmlWriter.Flush();

            ThrowIfFalse(xmlString.ToString() ==
@"<Example>
  <Widgets>
    <Item>
      <Widget>
        <Name>Konfabulator</Name>
        <Number>3.14</Number>
      </Widget>
    </Item>
  </Widgets>
</Example>");
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
