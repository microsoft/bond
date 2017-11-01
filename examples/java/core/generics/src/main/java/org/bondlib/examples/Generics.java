package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.BondTypes;
import org.bondlib.Deserializer;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Serializer;
import org.bondlib.StructBondType;

// See build.gradle for namespace mapping
import org.bondlib.examples.generics.Base;
import org.bondlib.examples.generics.Struct;

public class Generics {

    public static void main(final String[] args) throws IOException {

        // Create a Bond type descriptor for
        // the Bond struct Struct<string, double, Base<wstring>>. Explicit type
        // descriptors are needed when using generics to "fill in" the holes in
        // the generated generic type with real types.
        //
        // Note that java.lang.String is used for both Bond's string and
        // wstring types. BondTypes.STRING/WSTRING is used to distinguish
        // between the two.
        final StructBondType<Struct<String, Double, Base<String>>> structBondType =
                Struct.BOND_TYPE.makeGenericType(
                    BondTypes.STRING,
                    BondTypes.DOUBLE,
                    Base.BOND_TYPE.makeGenericType(BondTypes.WSTRING));

        // Since Struct is generic, when constructing it, a type descriptor
        // must be passed explicitly.
        //
        // Here, we pass a previously created type descriptor. Type
        // descriptors are cached, so Struct.BOND_TYPE.makeGenericType() could
        // be invoked directly in the constructor parameter list if wanted.
        final Struct<String, Double, Base<String>> obj = new Struct<>(structBondType);
        obj.n.x = "test";
        obj.x = 0.11;
        obj.y = "test";
        obj.items.add(3.14);
        obj.items.add(0D);

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Struct> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Struct<String, Double, Base<String>>> deserializer = new Deserializer<>(structBondType);
        final Struct<String, Double, Base<String>> obj2 = deserializer.deserialize(reader);

        assert obj.equals(obj2) : "Roundtrip failed";
    }
}
