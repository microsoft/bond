package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.Bonded;
import org.bondlib.Deserializer;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Serializer;

import org.bondlib.examples.polymorphiccontainer.Circle;
import org.bondlib.examples.polymorphiccontainer.Polymorphic;
import org.bondlib.examples.polymorphiccontainer.Rectangle;
import org.bondlib.examples.polymorphiccontainer.Shape;
import org.bondlib.examples.polymorphiccontainer.Type;

public class PolymorphicContainer {

    public static void main(final String[] args) throws IOException {

        final Circle circle = new Circle();
        circle.Type = Type.Circle;
        circle.Radius = 3.14;

        final Rectangle rectangle = new Rectangle();
        rectangle.Type = Type.Rectangle;
        rectangle.Width = 10.0;
        rectangle.Height = 5.5;

        final Polymorphic src = new Polymorphic();
        // We must explicitly upcast to a Bonded<Shape> with Bonded.cast()
        // to be able to insert into the collection.
        src.Shapes.add(Bonded.fromObject(circle, Circle.BOND_TYPE).cast(Shape.BOND_TYPE));
        src.Shapes.add(Bonded.fromObject(rectangle, Rectangle.BOND_TYPE).cast(Shape.BOND_TYPE));

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Polymorphic> serializer = new Serializer<>();
        serializer.serialize(src, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Polymorphic> deserializer = new Deserializer<>(Polymorphic.BOND_TYPE);
        final Polymorphic dst = deserializer.deserialize(reader);

        for (Bonded<Shape> item : dst.Shapes) {
            // Deserialize item as Shape and extract object type
            Type type = item.deserialize().Type;

            if (type.equals(Type.Circle)) {
                // The generic method
                // Bonded<T>.deserialize(StructBondType<U>) can be used to
                // deserialize as a specific type.
                final Circle c = item.deserialize(Circle.BOND_TYPE);
                assert circle.equals(c): "Circle roundtrip failed";
            } else if (type.equals(Type.Rectangle)) {
                // Alternatively, Bonded<T>.convert(StructBondType<U>) can
                // be used to get a Bonded<U>, and then
                // Bonded<U>.deserialize() can be used.
                final Rectangle r = item.convert(Rectangle.BOND_TYPE).deserialize();
                assert rectangle.equals(r): "Rectangle rounttip failed";
            }
        }
    }
}
