package org.bondlib;

import org.bondlib.test.FieldRemovedRecord;
import org.bondlib.test.RecordV1;
import org.bondlib.test.RecordV2;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class UntaggedWithSchemaDeserializationTests {
    private static final int SIMPLE_VERSION = 2;
    private RecordV1 recordIn;
    private SchemaDef recordV1Schema;
    private SimpleBinaryReader reader;

    /*
     * These tests all begin with serialized data containing the schema for
     * RecordV1 and a real RecordV1. The actual tests vary the type they
     * attempt to deserialize into.
     */
    @Before
    public void setup() throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final SimpleBinaryWriter writer = new SimpleBinaryWriter(baos, SIMPLE_VERSION);
        final Serializer<SchemaDef> schemaSerializer = new Serializer<SchemaDef>();
        final Serializer<RecordV1> recordSerializer = new Serializer<RecordV1>();
        recordIn = new RecordV1();
        recordIn.name = "event";
        recordIn.tstamp = 800;

        schemaSerializer.serialize(RecordV1.BOND_TYPE.buildSchemaDef(), writer);
        recordSerializer.serialize(recordIn, writer);

        final ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        reader = new SimpleBinaryReader(bais, SIMPLE_VERSION);
        final Deserializer<SchemaDef> schemaDeserializer = new Deserializer<SchemaDef>(SchemaDef.BOND_TYPE);
        recordV1Schema = schemaDeserializer.deserialize(reader);
    }

    @Test
    public void sameTypeAsSchema() throws IOException {
        final Deserializer<RecordV1> recordV1Deserializer = new Deserializer<RecordV1>(RecordV1.BOND_TYPE);
        final RecordV1 recordOut = recordV1Deserializer.deserialize(reader, recordV1Schema);

        Assert.assertEquals(recordIn, recordOut);
    }

    @Test
    public void fieldAdded() throws IOException {
        final Deserializer<RecordV2> recordV2Deserializer = new Deserializer<RecordV2>(RecordV2.BOND_TYPE);
        final RecordV2 recordOut = recordV2Deserializer.deserialize(reader, recordV1Schema);

        Assert.assertEquals(recordIn.name, recordOut.name);
        Assert.assertEquals(recordIn.tstamp, recordOut.tstamp);
        Assert.assertEquals("", recordOut.location);
    }

    @Test
    public void fieldRemoved() throws IOException {
        final Deserializer<FieldRemovedRecord> fieldRemovedRecordDeserializer =
            new Deserializer<FieldRemovedRecord>(FieldRemovedRecord.BOND_TYPE);
        final FieldRemovedRecord recordOut = fieldRemovedRecordDeserializer.deserialize(reader, recordV1Schema);

        Assert.assertEquals(recordIn.tstamp, recordOut.tstamp);
    }
}
