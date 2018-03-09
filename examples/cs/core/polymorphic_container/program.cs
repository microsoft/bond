namespace Examples
{
    using System;
    using System.Collections.Generic;
    
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            var circle = new Circle
            {
                Type = Type.Circle,
                Radius = 3.14
            };

            var rectangle = new Rectangle
            {
                Type = Type.Rectangle,
                Width = 10,
                Height = 5.5
            };

            var src = new Polymorphic
            {
                Shapes = 
                {
                    new Bonded<Circle>(circle),
                    new Bonded<Rectangle>(rectangle)
                }
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Polymorphic>.From(reader);

            var deserializers = new Dictionary<Type, Deserializer<CompactBinaryReader<InputBuffer>>>
            {
                {Type.Circle, new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Circle))},
                {Type.Rectangle, new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Rectangle))}
            };
            
            foreach (var item in dst.Shapes)
            {
                // Deserialize item as Shape and extract object type
                var type = item.Deserialize().Type;
                
                // Select one of the precreated deserializers based on the item type
                var shape = deserializers[type].Deserialize(item);
                
                if (shape.GetType() == typeof(Circle))
                {
                    ThrowIfFalse(Comparer.Equal(circle, shape as Circle));
                }

                if (shape.GetType() == typeof(Rectangle))
                {
                    ThrowIfFalse(Comparer.Equal(rectangle, shape as Rectangle));
                }

                // Alternatively the generic method IBonded<T>.Deserialize<U> can be used
                if (type == Type.Circle)
                {
                    var c = item.Deserialize<Circle>();
                    ThrowIfFalse(Comparer.Equal(circle, c));
                }

                if (type == Type.Rectangle)
                {
                    var r = item.Deserialize<Rectangle>();
                    ThrowIfFalse(Comparer.Equal(rectangle, r));
                }
            }
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
