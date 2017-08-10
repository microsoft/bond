package com.microsoft.bond;

import com.microsoft.bond.protocol.FastBinaryReader;
import com.microsoft.bond.protocol.FastBinaryWriter;
import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;

public class BondedTest {
    @Test
    public void bondedEquality() throws IOException {
        // Bonded.equals() must be reference equality (i.e., Object.equals())
        // for all subtypes. We'll make a set of bonded instances that could
        // reasonably be expected to be .equals(), but that we have defined not
        // to be.

        // For backing data, we use
        //   * a non-default struct, so .equals() isn't trivial
        //   * a serialized representation of that struct, which must .equals()
        //     the original instance
        //   * a single reader pointing at those bytes
        final GUID guid = new GUID();
        guid.Data1 = 0xDEADBEEF;

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final FastBinaryWriter writer = new FastBinaryWriter(output, (short) 1);
        new Serializer<GUID>().serialize(guid, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
        final FastBinaryReader reader = new FastBinaryReader(input, (short) 1);

        // Verify our assumption that the test type will be .equals() after a
        // round-trip.
        final GUID roundTripped = new Deserializer<GUID>(GUID.BOND_TYPE).deserialize(reader);
        Assert.assertTrue(guid.equals(roundTripped));
        input.reset();

        final ArrayList<Bonded> bondeds = new ArrayList<Bonded>();
        // Make two of each subtype of Bonded, with each pair backed by the
        // same objects.
        bondeds.add(Bonded.fromObject(guid));
        bondeds.add(Bonded.fromObject(guid));
        bondeds.add(Bonded.fromObject(guid, GUID.BOND_TYPE));
        bondeds.add(Bonded.fromObject(guid, GUID.BOND_TYPE));
        bondeds.add(Bonded.fromProtocolReader(reader));
        bondeds.add(Bonded.fromProtocolReader(reader));
        bondeds.add(Bonded.fromProtocolReader(reader, GUID.BOND_TYPE));
        bondeds.add(Bonded.fromProtocolReader(reader, GUID.BOND_TYPE));

        // Exhaustively test equality.
        for (int i = 0; i < bondeds.size(); i++) {
            final Bonded one = bondeds.get(i);
            for (int j = 0; j < bondeds.size(); j++) {
                final Bonded another = bondeds.get(j);
                if (i == j) {
                    // Same instance - should be .equals().
                    Assert.assertTrue(
                        String.format("%s.equals(%s) was false; should have been true", one, another),
                        one.equals(another));
                } else {
                    // Different instances - should not be .equals().
                    Assert.assertFalse(
                        String.format("%s.equals(%s) was true; should have been false", one, another),
                        one.equals(another));
                }
            }
        }
    }
}
