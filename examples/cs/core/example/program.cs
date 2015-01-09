namespace Examples
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Text;
    using System.Xml;
    using Bond;
    using Bond.Tag;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    // Sample class representing Bond schema (usually generated from Bond IDL)
    // Equivalent definition in Bond IDL:
    // struct Example
    // {
    //      0: double Number = 3.14;
    //      1: nullable<vector<wstring>> Items;
    // }
    [Schema]
    public class Example
    {
        [Id(0)]
        public double Number { get; set; }

        // The optional Bond.Type attribute is used to express Bond schema metadata that
        // can't be directly inferred from .NET type. In this case it specifies that the field
        // is nullable, that the concrete type for IList<T> is List<T> and the string is UTF16.
        [Id(1), Type(typeof(nullable<List<wstring>>))]
        public IList<string> Items { get; set; }

        public Example()
        {
            Number = 3.14;
        }
    }

    [Schema]
    public class Example2
    {
        [Id(0)] 
        public double Number = 3.14;
    }

    static class Program
    {
        static void Main()
        {
            var src = new Example {Number = 6.28, Items = new List<string> {"foo", "bar"}};

            // Basic de/serialization/transcoding/cloning using static APIs which
            // internally cache generated Serializer/Deserializer/Transcoder/Cloner.
            using (var stream = new MemoryStream())
            {
                // De/serialization using Compact Binary protocol
                var output = new OutputStream(stream);
                var writer = new CompactBinaryWriter<OutputStream>(output);

                Serialize.To(writer, src);

                output.Flush();
                stream.Position = 0;

                var input = new InputStream(stream);
                var reader = new CompactBinaryReader<InputStream>(input);
                
                var dst = Deserialize<Example>.From(reader);

                Debug.Assert(Comparer.Equal(src, dst));

                // Transcoding Compact Binary payload to Xml string
                stream.Position = 0;
                input = new InputStream(stream);
                reader = new CompactBinaryReader<InputStream>(input);
                var builder = new StringBuilder();
                var xml = new SimpleXmlWriter(XmlWriter.Create(builder, new XmlWriterSettings
                {
                    OmitXmlDeclaration = true,
                    Indent = true
                }));
                Transcode<Example>.FromTo(reader, xml);
                xml.Flush();
                Console.WriteLine(builder.ToString());

                // Cloning an object into a different, compatible type
                var clone = Clone<Example2>.From(src);
                Debug.Assert(src.GetType() == typeof(Example));
                Debug.Assert(clone.GetType() == typeof(Example2));
                Debug.Assert(clone.Number == src.Number);
            }

            // Access runtime schema for a given type
            var schemaExample = Schema<Example>.RuntimeSchema;
            var schemaExample2 = Schema.GetRuntimeSchema(typeof(Example2));
            
            // Explicit instantiation of Serializer/Deserializer/Transcoder/Cloner which 
            // keep the generated delegates implementing Bond functionality.

            // Serializer of Example objects to Compact Binary protocol
            var serializer = new Serializer<
                CompactBinaryWriter<OutputStream>>(typeof(Example));

            // Transcoder from Compact Binary protocol to Simple protocol using specified schema
            var transcoder = new Transcoder<
                CompactBinaryReader<InputStream>,
                SimpleBinaryWriter<OutputStream>>(schemaExample);

            // Deserializer for object of type Example2 from Simple protocol payload with specified schema.
            // The runtime schema is needed in order to transcode to untagged protocol, like Simple here,
            // or text protocol like Xml.
            var deserializer = new Deserializer<
                SimpleBinaryReader<InputStream>>(typeof(Example2), schemaExample);

            using (var stream = new MemoryStream())
            using (var stream2 = new MemoryStream())
            {
                var output = new OutputStream(stream);
                var writer = new CompactBinaryWriter<OutputStream>(output);

                serializer.Serialize(src, writer);

                output.Flush();
                stream.Position = 0;

                var input = new InputStream(stream);
                var reader = new CompactBinaryReader<InputStream>(input);

                var output2 = new OutputStream(stream2);
                var writer2 = new SimpleBinaryWriter<OutputStream>(output2);

                transcoder.Transcode(reader, writer2);

                output2.Flush();
                stream2.Position = 0;

                var input2 = new InputStream(stream2);
                var reader2 = new SimpleBinaryReader<InputStream>(input2);

                var dst = deserializer.Deserialize<Example2>(reader2);
                Debug.Assert(dst.Number == src.Number);
            }
        }
    }
}
