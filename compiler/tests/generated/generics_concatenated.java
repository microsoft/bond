
package tests;


@javax.annotation.Generated("gbc")
public class Foo<T1, T2> implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    private static final com.microsoft.bond.FieldDef t2_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef n_FIELD_DEF = new com.microsoft.bond.FieldDef();
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

        t2_FIELD_DEF.metadata.name = "t2";
        t2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        t2_FIELD_DEF.id = 0;
        t2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        t2_FIELD_DEF.type.struct_def = 0;
        t2_FIELD_DEF.type.element = null;
        t2_FIELD_DEF.type.key = null;
        t2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(t2_FIELD_DEF);

        n_FIELD_DEF.metadata.name = "n";
        n_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        n_FIELD_DEF.id = 1;
        n_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_LIST;
        n_FIELD_DEF.type.struct_def = 0;
        n_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        n_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_STRUCT;
        n_FIELD_DEF.type.element.struct_def = 0;
        n_FIELD_DEF.type.element.element = null;
        n_FIELD_DEF.type.element.key = null;
        n_FIELD_DEF.type.element.bonded_type = false;
        n_FIELD_DEF.type.key = null;
        n_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(n_FIELD_DEF);

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

    public T2 t2 = null;

    public Foo<T1, Boolean> n = null;

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
        
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 0, t2_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_LIST, 1, n_FIELD_DEF.metadata);
        if (this.n == null) {
            writer.writeContainerBegin(0, com.microsoft.bond.BondDataType.BT_STRUCT);
        } else {
            writer.writeContainerBegin(1, com.microsoft.bond.BondDataType.BT_STRUCT);
            this.n.serialize(writer);
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readListBegin(this.__readContainerResult);
        if (this.__readContainerResult.count == 0) {
            this.n = null;
        } else {
            this.n = new Foo<T1, Boolean>(); this.n.deserialize(reader);
        }
        reader.readContainerEnd();
        reader.readFieldEnd();
        
    }

    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }
}
