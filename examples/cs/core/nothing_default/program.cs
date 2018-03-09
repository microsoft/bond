namespace Examples
{
    using System;
    using System.Collections.Generic;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using examples.nothing_default;

    static class Program
    {
        static void Main()
        {
            var v1 = new Struct_v1();

            // Struct_v1 has a required field foo which by default is set to
            // 'nothing'. If we try to serialize object v1 w/o initializing
            // the field to some value Bond will throw an exception.
            try
            {
                Console.WriteLine("Serializing v1... ");
                Marshal(v1);
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex);
            }

            // Initialize field by assigning a value to it...
            v1.foo = 10;

            // ... or for complex fields create a new instance of the
            // appropriate C# type.
            v1.baz = new LinkedList<string>();
            v1.baz.AddLast("test1");
            v1.baz.AddLast("test2");

            // We can also set a field to 'nothing' by assigning null to the
            // field itself. Optional fields that are set to 'nothing' are
            // omitted when object is serialized.
            v1.baz = null;

            ArraySegment<byte> buffer = Marshal(v1);

            // Deserialize the payload into object of type Struct_v2
            Struct_v2 v2 = Unmarshal<Struct_v2>.From(buffer);

            // Struct_v2 has an optional field bar, which didn't exist in
            // Struct_v1. It is initialized to 'nothing' by default. By
            // checking if the field is 'nothing' (== null) after
            // de-serialization we can detect if it was present in the
            // payload or not.
            ThrowIfFalse(v2.bar == null);
            ThrowIfFalse(v2.baz == null);
        }

        static ArraySegment<byte> Marshal<T>(T obj)
        {
            var buffer = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(buffer);

            Bond.Marshal.To(writer, obj);

            return buffer.Data;
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
