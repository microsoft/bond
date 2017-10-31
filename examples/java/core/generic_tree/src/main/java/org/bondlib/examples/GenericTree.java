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
import org.bondlib.examples.generictree.Node;

public class GenericTree {

    public static void main(final String[] args) throws IOException {

        // Type descriptor for the Bond struct Node<string>. Used to "fill in"
        // the holes in the generated generic type with real types.
        final StructBondType<Node<String>> nodeStringBondType = Node.BOND_TYPE.makeGenericType(BondTypes.STRING);

        // Define root node for a tree of strings.
        final Node<String> root = new Node<String>(nodeStringBondType);
        root.data = "root";

        root.left = new Node<String>(nodeStringBondType);
        root.left.data = "root/left";

        root.right = new Node<String>(nodeStringBondType);
        root.right.data = "root/right";

        root.left.left = new Node<String>(nodeStringBondType);
        root.left.left.data = "root/left/left";

        root.left.left.right = new Node<String>(nodeStringBondType);
        root.left.left.right.data = "root/left/left/right";

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Node> serializer = new Serializer<>();
        serializer.serialize(root, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Node<String>> deserializer = new Deserializer<>(nodeStringBondType);
        final Node<String> tree = deserializer.deserialize(reader);

        assert root.equals(tree) : "Roundtrip failed";
    }
}
