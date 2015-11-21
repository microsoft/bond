namespace CompatibilityTest
{
    using System;
    using System.IO;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using unittest.compat;

    static class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 3)
            {
                Console.WriteLine("Usage:\nBond.CompatibilityTest json|compact|compact2|fast|simple|simple2|schema input_file output_file [json|compact|fast|simple|simple2]");
                return;
            }

            var fromProtocol = args[0];
            var toProtocol = fromProtocol;
            var inputFile = args[1];
            var outputFile = args[2];

            if (args.Length == 4)
            {
                toProtocol = args[3];
            }

            using (var inputStream = new FileStream(inputFile, FileMode.Open, FileAccess.Read))
            {
                var input = new InputStream(inputStream);
                using (var outputStream = new FileStream(outputFile, FileMode.Create))
                {
                    var output = new OutputStream(outputStream);
                    if (fromProtocol == "json")
                    {
                        var reader = new SimpleJsonReader(inputStream);
                        var writer = new SimpleJsonWriter(outputStream);
                        var transcoder = new Transcoder<SimpleJsonReader, SimpleJsonWriter>(Schema<Compat>.RuntimeSchema);
                        transcoder.Transcode(reader, writer);
                        writer.Flush();
                    }
                    else if (fromProtocol == "compact")
                    {
                        var reader = new CompactBinaryReader<InputStream>(input);
                        Write(reader, output, toProtocol);
                    }
                    else if (fromProtocol == "compact2")
                    {
                        var reader = new CompactBinaryReader<InputStream>(input, 2);
                        Write(reader, output, toProtocol);
                    }
                    else if (fromProtocol == "fast")
                    {
                        var reader = new FastBinaryReader<InputStream>(input, 2);
                        Write(reader, output, toProtocol);
                    }
                    else if (fromProtocol == "simple")
                    {
                        var reader = new SimpleBinaryReader<InputStream>(input);
                        Write(reader, output, toProtocol);
                    }
                    else if (fromProtocol == "simple2")
                    {
                        var reader = new SimpleBinaryReader<InputStream>(input, 2);
                        Write(reader, output, toProtocol);
                    }
                    else if (fromProtocol == "schema")
                    {
                        SchemaDef schema;

                        var c = (char)inputStream.ReadByte();
                        inputStream.Seek(0, SeekOrigin.Begin);

                        if (c == '{')
                        {
                            var reader = new SimpleJsonReader(inputStream);
                            schema = Deserialize<SchemaDef>.From(reader);
                        }
                        else
                        {
                            schema = Unmarshal<SchemaDef>.From(input);   
                        }

                        if (!Comparer.Equal(schema, Schema<Compat>.RuntimeSchema.SchemaDef))
                        {
                            Console.WriteLine("SchemaDef is different");
                        }
                        var writer = new CompactBinaryWriter<OutputStream>(output);
                        Marshal.To(writer, Schema<Compat>.RuntimeSchema.SchemaDef);
                        output.Flush();
                    }
                    else
                    {
                        Console.WriteLine("Unsupported input protocol {0}", fromProtocol);
                    }
                }
            }
        }

        static void Write<R>(R reader, OutputStream output, string toProtocol)
        {
            if (toProtocol == "compact")
            {
                var writer = new CompactBinaryWriter<OutputStream>(output);
                Serialize.To(writer, Deserialize<Compat>.From(reader));
            }
            else if (toProtocol == "fast")
            {
                var writer = new FastBinaryWriter<OutputStream>(output);
                Serialize.To(writer, Deserialize<Compat>.From(reader));
            }
            else if (toProtocol == "simple")
            {
                var writer = new SimpleBinaryWriter<OutputStream>(output);
                Serialize.To(writer, Deserialize<Compat>.From(reader));
            }
            else if (toProtocol == "simple2")
            {
                var writer = new SimpleBinaryWriter<OutputStream>(output, 2);
                Serialize.To(writer, Deserialize<Compat>.From(reader));
            }
            else
            {
                Console.WriteLine("Unsupported output protocol {0}", toProtocol);
            }
            output.Flush();
        }
    }
}
