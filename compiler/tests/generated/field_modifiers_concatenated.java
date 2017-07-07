
package tests;


@javax.annotation.Generated("gbc")
public class Foo implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    private static final com.microsoft.bond.FieldDef o_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef r_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef ro_FIELD_DEF = new com.microsoft.bond.FieldDef();
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
        
        // TODO: .base_def
        SCHEMA.structs.add(0, STRUCT_DEF);

        o_FIELD_DEF.metadata.name = "o";
        o_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        o_FIELD_DEF.id = 0;
        o_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_BOOL;
        o_FIELD_DEF.type.struct_def = 0;
        o_FIELD_DEF.type.element = null;
        o_FIELD_DEF.type.key = null;
        o_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(o_FIELD_DEF);

        r_FIELD_DEF.metadata.name = "r";
        r_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        r_FIELD_DEF.id = 1;
        r_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT16;
        r_FIELD_DEF.type.struct_def = 0;
        r_FIELD_DEF.type.element = null;
        r_FIELD_DEF.type.key = null;
        r_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(r_FIELD_DEF);

        ro_FIELD_DEF.metadata.name = "ro";
        ro_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        ro_FIELD_DEF.id = 2;
        ro_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_DOUBLE;
        ro_FIELD_DEF.type.struct_def = 0;
        ro_FIELD_DEF.type.element = null;
        ro_FIELD_DEF.type.key = null;
        ro_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(ro_FIELD_DEF);

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

    public boolean o = false;

    public short r = 0;

    public double ro = 0.0;

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
        
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_BOOL, 0, o_FIELD_DEF.metadata);
        writer.writeBool(this.o);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT16, 1, r_FIELD_DEF.metadata);
        writer.writeInt16(this.r);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_DOUBLE, 2, ro_FIELD_DEF.metadata);
        writer.writeDouble(this.ro);
        writer.writeFieldEnd();
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
        reader.readFieldBegin(this.__readFieldResult);
        this.o = reader.readBool();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.r = reader.readInt16();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.ro = reader.readDouble();
        reader.readFieldEnd();
        
    }

    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }
}
