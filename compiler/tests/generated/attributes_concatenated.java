
package tests;


@javax.annotation.Generated("gbc")
public final class Enum implements com.microsoft.bond.BondEnum, java.lang.Comparable<Enum> {

    public static final class Values {
        private Values() {}

        public static final int Value1 = 0;
    }

    public static final Enum Value1 = new Enum(Values.Value1, "Value1");

    public final int value;

    private final String label;

    private Enum(int value, String label) { this.value = value; this.label = label; }

    @Override
    public int getValue() { return this.value; }

    @Override
    public int compareTo(Enum o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public boolean equals(Object other) { return (other instanceof Enum) && (this.value == ((Enum) other).value); }

    @Override
    public int hashCode() { return this.value; }

    @Override
    public String toString() { return this.label != null ? this.label : ("Enum(" + String.valueOf(this.value) + ")"); }

    public static Enum get(int value) {
        switch (value) {
            case Values.Value1: return Value1;
            default: return new Enum(value, null);
        }
    }
}

package tests;


@javax.annotation.Generated("gbc")
public class Foo implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    private static final com.microsoft.bond.FieldDef f_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static boolean schemaInitialized = false;

    public static synchronized void initSchema() {
        if (schemaInitialized) { return; }

        

        SCHEMA.root.id = com.microsoft.bond.BondDataType.BT_STRUCT;
        SCHEMA.root.struct_def = 0;
        SCHEMA.root.element = null;
        SCHEMA.root.key = null;
        SCHEMA.root.bonded_type = false;

        STRUCT_DEF.metadata.name = "Foo";
        STRUCT_DEF.metadata.qualified_name = "tests.Foo";
        STRUCT_DEF.metadata.modifier = com.microsoft.bond.Modifier.Optional;
        STRUCT_DEF.metadata.attributes.put("StructAttribute1", "one");
        STRUCT_DEF.metadata.attributes.put("StructAttribute2", "two");
        // TODO: .base_def
        SCHEMA.structs.add(0, STRUCT_DEF);

        f_FIELD_DEF.metadata.name = "f";
        f_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        f_FIELD_DEF.metadata.attributes.put("FieldAttribute1", "one");
        f_FIELD_DEF.metadata.attributes.put("FieldAttribute2", "two");
        f_FIELD_DEF.id = 0;
        f_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_STRING;
        f_FIELD_DEF.type.struct_def = 0;
        f_FIELD_DEF.type.element = null;
        f_FIELD_DEF.type.key = null;
        f_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(f_FIELD_DEF);

        schemaInitialized = true;
    }

    public static com.microsoft.bond.SchemaDef getSchema() {
        initSchema();
        return SCHEMA;
    }

    public static com.microsoft.bond.StructDef getStructDef() {
        initSchema();
        return STRUCT_DEF;
    }
    

    private final com.microsoft.bond.protocol.TaggedProtocolReader.ReadFieldResult __readFieldResult = new com.microsoft.bond.protocol.TaggedProtocolReader.ReadFieldResult();
    private final com.microsoft.bond.protocol.TaggedProtocolReader.ReadContainerResult __readContainerResult = new com.microsoft.bond.protocol.TaggedProtocolReader.ReadContainerResult();

    public String f = "";

    @Override
    public void serialize(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        initSchema();

        writer.writeStructBegin(SCHEMA.structs.get(0).metadata);
        this.serializeFields(writer);
        writer.writeStructEnd();
    }

    @Override
    public void deserialize(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        initSchema();

        reader.readStructBegin();
        this.deserializeFields(reader);
        reader.readStructEnd();
    }

    protected void serializeFields(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_STRING, 0, f_FIELD_DEF.metadata);
        writer.writeString(this.f);
        writer.writeFieldEnd();
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
        reader.readFieldBegin(this.__readFieldResult);
        this.f = reader.readString();
        reader.readFieldEnd();
        
    }

    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }
}
