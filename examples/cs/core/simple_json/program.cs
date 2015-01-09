namespace Examples
{
    using System;
    using System.IO;
    using System.Text;
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

            var jsonString = new StringBuilder();
            var jsonWriter = new SimpleJsonWriter(new StringWriter(jsonString));

            Serialize.To(jsonWriter, config);
            jsonWriter.Flush();
            Console.WriteLine(jsonString);

            var reader = new SimpleJsonReader(new StringReader(jsonString.ToString()));
            config = Deserialize<Config>.From(reader);
        }
    }
}
