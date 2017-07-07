
package tests;


@javax.annotation.Generated("gbc")
public class Foo implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    
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
        
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
    }

    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }
}

package tests;


@javax.annotation.Generated("gbc")
public class ComplexTypes implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    private static final com.microsoft.bond.FieldDef li8_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef sb_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef vb_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef nf_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef msws_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef bfoo_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static boolean schemaInitialized = false;

    public static synchronized void initSchema() {
        if (schemaInitialized) { return; }

        

        SCHEMA.root.id = com.microsoft.bond.BondDataType.BT_STRUCT;
        SCHEMA.root.struct_def = 0;
        SCHEMA.root.element = null;
        SCHEMA.root.key = null;
        SCHEMA.root.bonded_type = false;

        STRUCT_DEF.metadata.name = "ComplexTypes";
        STRUCT_DEF.metadata.qualified_name = "tests.ComplexTypes";
        STRUCT_DEF.metadata.modifier = com.microsoft.bond.Modifier.Optional;
        
        // TODO: .base_def
        SCHEMA.structs.add(0, STRUCT_DEF);

        li8_FIELD_DEF.metadata.name = "li8";
        li8_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        li8_FIELD_DEF.id = 0;
        li8_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_LIST;
        li8_FIELD_DEF.type.struct_def = 0;
        li8_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        li8_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_INT8;
        li8_FIELD_DEF.type.element.struct_def = 0;
        li8_FIELD_DEF.type.element.element = null;
        li8_FIELD_DEF.type.element.key = null;
        li8_FIELD_DEF.type.element.bonded_type = false;
        li8_FIELD_DEF.type.key = null;
        li8_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(li8_FIELD_DEF);

        sb_FIELD_DEF.metadata.name = "sb";
        sb_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        sb_FIELD_DEF.id = 1;
        sb_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_SET;
        sb_FIELD_DEF.type.struct_def = 0;
        sb_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        sb_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_BOOL;
        sb_FIELD_DEF.type.element.struct_def = 0;
        sb_FIELD_DEF.type.element.element = null;
        sb_FIELD_DEF.type.element.key = null;
        sb_FIELD_DEF.type.element.bonded_type = false;
        sb_FIELD_DEF.type.key = null;
        sb_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(sb_FIELD_DEF);

        vb_FIELD_DEF.metadata.name = "vb";
        vb_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        vb_FIELD_DEF.id = 2;
        vb_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_LIST;
        vb_FIELD_DEF.type.struct_def = 0;
        vb_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        vb_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_LIST;
        vb_FIELD_DEF.type.element.struct_def = 0;
        vb_FIELD_DEF.type.element.element = null;
        vb_FIELD_DEF.type.element.key = null;
        vb_FIELD_DEF.type.element.bonded_type = false;
        vb_FIELD_DEF.type.key = null;
        vb_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(vb_FIELD_DEF);

        nf_FIELD_DEF.metadata.name = "nf";
        nf_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        nf_FIELD_DEF.id = 3;
        nf_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_LIST;
        nf_FIELD_DEF.type.struct_def = 0;
        nf_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        nf_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_FLOAT;
        nf_FIELD_DEF.type.element.struct_def = 0;
        nf_FIELD_DEF.type.element.element = null;
        nf_FIELD_DEF.type.element.key = null;
        nf_FIELD_DEF.type.element.bonded_type = false;
        nf_FIELD_DEF.type.key = null;
        nf_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(nf_FIELD_DEF);

        msws_FIELD_DEF.metadata.name = "msws";
        msws_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        msws_FIELD_DEF.id = 4;
        msws_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_MAP;
        msws_FIELD_DEF.type.struct_def = 0;
        msws_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        msws_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_WSTRING;
        msws_FIELD_DEF.type.element.struct_def = 0;
        msws_FIELD_DEF.type.element.element = null;
        msws_FIELD_DEF.type.element.key = null;
        msws_FIELD_DEF.type.element.bonded_type = false;
        msws_FIELD_DEF.type.key = new com.microsoft.bond.TypeDef();
        msws_FIELD_DEF.type.key.id = com.microsoft.bond.BondDataType.BT_STRING;
        msws_FIELD_DEF.type.key.struct_def = 0;
        msws_FIELD_DEF.type.key.element = null;
        msws_FIELD_DEF.type.key.key = null;
        msws_FIELD_DEF.type.key.bonded_type = false;
        msws_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(msws_FIELD_DEF);

        bfoo_FIELD_DEF.metadata.name = "bfoo";
        bfoo_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        bfoo_FIELD_DEF.id = 5;
        bfoo_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        bfoo_FIELD_DEF.type.struct_def = 0;
        bfoo_FIELD_DEF.type.element = null;
        bfoo_FIELD_DEF.type.key = null;
        bfoo_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(bfoo_FIELD_DEF);

        m_FIELD_DEF.metadata.name = "m";
        m_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_FIELD_DEF.id = 6;
        m_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_MAP;
        m_FIELD_DEF.type.struct_def = 0;
        m_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_LIST;
        m_FIELD_DEF.type.element.struct_def = 0;
        m_FIELD_DEF.type.element.element = new com.microsoft.bond.TypeDef();
        m_FIELD_DEF.type.element.element.id = com.microsoft.bond.BondDataType.BT_LIST;
        m_FIELD_DEF.type.element.element.struct_def = 0;
        m_FIELD_DEF.type.element.element.element = new com.microsoft.bond.TypeDef();
        m_FIELD_DEF.type.element.element.element.id = com.microsoft.bond.BondDataType.BT_LIST;
        m_FIELD_DEF.type.element.element.element.struct_def = 0;
        m_FIELD_DEF.type.element.element.element.element = new com.microsoft.bond.TypeDef();
        m_FIELD_DEF.type.element.element.element.element.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_FIELD_DEF.type.element.element.element.element.struct_def = 0;
        m_FIELD_DEF.type.element.element.element.element.element = null;
        m_FIELD_DEF.type.element.element.element.element.key = null;
        m_FIELD_DEF.type.element.element.element.element.bonded_type = false;
        m_FIELD_DEF.type.element.element.element.key = null;
        m_FIELD_DEF.type.element.element.element.bonded_type = false;
        m_FIELD_DEF.type.element.element.key = null;
        m_FIELD_DEF.type.element.element.bonded_type = false;
        m_FIELD_DEF.type.element.key = null;
        m_FIELD_DEF.type.element.bonded_type = false;
        m_FIELD_DEF.type.key = new com.microsoft.bond.TypeDef();
        m_FIELD_DEF.type.key.id = com.microsoft.bond.BondDataType.BT_DOUBLE;
        m_FIELD_DEF.type.key.struct_def = 0;
        m_FIELD_DEF.type.key.element = null;
        m_FIELD_DEF.type.key.key = null;
        m_FIELD_DEF.type.key.bonded_type = false;
        m_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_FIELD_DEF);

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

    public java.util.List<Byte> li8 = new java.util.LinkedList();

    public java.util.Set<Boolean> sb = new java.util.HashSet();

    public java.util.List<byte[]> vb = new java.util.ArrayList();

    public Float nf = null;

    public java.util.Map<String, String> msws = new java.util.HashMap();

    public com.microsoft.bond.IBonded<Foo> bfoo = new com.microsoft.bond.Bonded<Foo>(new Foo());

    public java.util.Map<Double, java.util.List<java.util.List<com.microsoft.bond.IBonded<Bar>>>> m = new java.util.HashMap();

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
        
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_LIST, 0, li8_FIELD_DEF.metadata);
        writer.writeContainerBegin(this.li8.size(), com.microsoft.bond.BondDataType.BT_INT8);
        for (byte e0 : this.li8) {
            writer.writeInt8(e0);
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_SET, 1, sb_FIELD_DEF.metadata);
        writer.writeContainerBegin(this.sb.size(), com.microsoft.bond.BondDataType.BT_BOOL);
        for (boolean e0 : this.sb) {
            writer.writeBool(e0);
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_LIST, 2, vb_FIELD_DEF.metadata);
        writer.writeContainerBegin(this.vb.size(), com.microsoft.bond.BondDataType.BT_LIST);
        for (byte[] e0 : this.vb) {
            writer.writeContainerBegin(e0.length, com.microsoft.bond.BondDataType.BT_INT8);
        writer.writeBytes(e0);
        writer.writeContainerEnd();
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_LIST, 3, nf_FIELD_DEF.metadata);
        if (this.nf == null) {
            writer.writeContainerBegin(0, com.microsoft.bond.BondDataType.BT_FLOAT);
        } else {
            writer.writeContainerBegin(1, com.microsoft.bond.BondDataType.BT_FLOAT);
            writer.writeFloat(this.nf);
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_MAP, 4, msws_FIELD_DEF.metadata);
        writer.writeContainerBegin(this.msws.size(), com.microsoft.bond.BondDataType.BT_STRING, com.microsoft.bond.BondDataType.BT_WSTRING);
        for (java.util.Map.Entry<String, String> e0 : this.msws.entrySet()) {
            writer.writeString(e0.getKey());
            writer.writeWString(e0.getValue());
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 5, bfoo_FIELD_DEF.metadata);
        this.bfoo.serialize(writer);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_MAP, 6, m_FIELD_DEF.metadata);
        writer.writeContainerBegin(this.m.size(), com.microsoft.bond.BondDataType.BT_DOUBLE, com.microsoft.bond.BondDataType.BT_LIST);
        for (java.util.Map.Entry<Double, java.util.List<java.util.List<com.microsoft.bond.IBonded<Bar>>>> e0 : this.m.entrySet()) {
            writer.writeDouble(e0.getKey());
            writer.writeContainerBegin(e0.getValue().size(), com.microsoft.bond.BondDataType.BT_LIST);
        for (java.util.List<com.microsoft.bond.IBonded<Bar>> e1 : e0.getValue()) {
            writer.writeContainerBegin(e1.size(), com.microsoft.bond.BondDataType.BT_LIST);
        for (com.microsoft.bond.IBonded<Bar> e2 : e1) {
            if (e2 == null) {
            writer.writeContainerBegin(0, com.microsoft.bond.BondDataType.BT_UNAVAILABLE);
        } else {
            writer.writeContainerBegin(1, com.microsoft.bond.BondDataType.BT_UNAVAILABLE);
            e2.serialize(writer);
        }
        writer.writeContainerEnd();
        }
        writer.writeContainerEnd();
        }
        writer.writeContainerEnd();
        }
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readListBegin(this.__readContainerResult);
        {
            long count0 = this.__readContainerResult.count;
            for (long i0 = 0; i0 < count0; i0++) {
                byte e0 = 0;
                e0 = reader.readInt8();
                this.li8.add(e0);
            }
        }
        reader.readContainerEnd();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readListBegin(this.__readContainerResult);
        {
            long count0 = this.__readContainerResult.count;
            for (long i0 = 0; i0 < count0; i0++) {
                boolean e0 = false;
                e0 = reader.readBool();
                this.sb.add(e0);
            }
        }
        reader.readContainerEnd();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readListBegin(this.__readContainerResult);
        {
            long count0 = this.__readContainerResult.count;
            for (long i0 = 0; i0 < count0; i0++) {
                byte[] e0 = new byte[0];
                reader.readListBegin(this.__readContainerResult);
        e0 = reader.readBytes(__readContainerResult.count);
        reader.readContainerEnd();
                this.vb.add(e0);
            }
        }
        reader.readContainerEnd();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readListBegin(this.__readContainerResult);
        if (this.__readContainerResult.count == 0) {
            this.nf = null;
        } else {
            this.nf = reader.readFloat();
        }
        reader.readContainerEnd();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readMapBegin(__readContainerResult);
        {
            long count0 = __readContainerResult.count;
            for (long i0 = 0; i0 < count0; i0++) {
                String k0 = "";
                String v0 = "";
                k0 = reader.readString();
                v0 = reader.readWString();
                this.msws.put(k0, v0);
            }
        }
        reader.readContainerEnd();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        {
            final Foo t = new Foo();
            t.deserialize(reader);
            this.bfoo = new com.microsoft.bond.Bonded<Foo>(t);
        }
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readMapBegin(__readContainerResult);
        {
            long count0 = __readContainerResult.count;
            for (long i0 = 0; i0 < count0; i0++) {
                double k0 = 0.0;
                java.util.List<java.util.List<com.microsoft.bond.IBonded<Bar>>> v0 = new java.util.LinkedList();
                k0 = reader.readDouble();
                reader.readListBegin(this.__readContainerResult);
        {
            long count1 = this.__readContainerResult.count;
            for (long i1 = 0; i1 < count1; i1++) {
                java.util.List<com.microsoft.bond.IBonded<Bar>> e1 = new java.util.ArrayList();
                reader.readListBegin(this.__readContainerResult);
        {
            long count2 = this.__readContainerResult.count;
            for (long i2 = 0; i2 < count2; i2++) {
                com.microsoft.bond.IBonded<Bar> e2 = null;
                reader.readListBegin(this.__readContainerResult);
        if (this.__readContainerResult.count == 0) {
            e2 = null;
        } else {
            {
            final Bar t = new Bar();
            t.deserialize(reader);
            e2 = new com.microsoft.bond.Bonded<Bar>(t);
        }
        }
        reader.readContainerEnd();
                e1.add(e2);
            }
        }
        reader.readContainerEnd();
                v0.add(e1);
            }
        }
        reader.readContainerEnd();
                this.m.put(k0, v0);
            }
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
