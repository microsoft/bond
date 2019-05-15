namespace Examples
{
    using System;
    using System.Collections.Generic;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using System.IO;

    static class Program
    {
        static public ArraySegment<byte> Pack<T>(T obj)
        {
            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, obj);

            return output.Data;
        }

        static void AssertValidate(SchemaValidation<CompactBinaryReader<InputBuffer>> sv, CompactBinaryReader<InputBuffer> reader)
        {
            sv.Validate(reader);
        }
        static void AssertValidateThrows<EX>(SchemaValidation<CompactBinaryReader<InputBuffer>> sv, CompactBinaryReader<InputBuffer> reader) where EX : Exception
        {
            try
            {
                sv.Validate(reader);
                throw new Exception("Assertion failed");
            }
            catch (EX)
            {
                // ignore
            }
        }

        static CompactBinaryReader<InputBuffer> CreateReader(ArraySegment<byte> data)
            => new CompactBinaryReader<InputBuffer>(new InputBuffer(data));

        static void Main()
        {
            var schema1 = Schema<Example1>.RuntimeSchema;
            var schema2 = Schema<Example2>.RuntimeSchema;
            var schema3 = Schema<Example3>.RuntimeSchema;
            var schema4 = Schema<Example4>.RuntimeSchema;

            {
                var src = new Example1
                {
                    Enabled = true,
                    Name = "Foo"
                };

                var data = Pack(src);

                var sv1 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema1);
                var sv2 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema2);
                var sv3 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema3);
                var sv4 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema4);

                AssertValidate(sv1, CreateReader(data));
                AssertValidate(sv2, CreateReader(data));
                AssertValidateThrows<InvalidDataException>(sv3, CreateReader(data));
                AssertValidate(sv4, CreateReader(data));
            }

            {
                var src = new Example3
                {
                    Enabled = true,
                    Name = "Foo",
                    Number = 123
                };

                var data = Pack(src);

                var sv1 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema1);
                var sv2 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema2);
                var sv3 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema3);
                var sv4 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema4);

                AssertValidateThrows<InvalidDataException>(sv1, CreateReader(data));
                AssertValidate(sv2, CreateReader(data));
                AssertValidate(sv3, CreateReader(data));
                AssertValidate(sv4, CreateReader(data));
            }

            {
                var src = new Example4
                {
                    Enabled = true,
                    Name = "Foo",
                    Number = 123
                };

                var data = Pack(src);

                var sv1 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema1);
                var sv2 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema2);
                var sv3 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema3);
                var sv4 = new SchemaValidation<CompactBinaryReader<InputBuffer>>(schema4);

                AssertValidateThrows<InvalidDataException>(sv1, CreateReader(data));
                AssertValidate(sv2, CreateReader(data));
                AssertValidate(sv3, CreateReader(data));
                AssertValidate(sv4, CreateReader(data));
            }

        }

    }
}
