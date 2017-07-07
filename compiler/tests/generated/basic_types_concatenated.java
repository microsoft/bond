
package tests;


@javax.annotation.Generated("gbc")
public class BasicTypes implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    private static final com.microsoft.bond.FieldDef _bool_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _str_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _wstr_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _uint64_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _uint16_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _uint32_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _uint8_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _int8_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _int16_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _int32_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _int64_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _double_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _float_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef _blob_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static boolean schemaInitialized = false;

    public static synchronized void initSchema() {
        if (schemaInitialized) { return; }

        

        SCHEMA.root.id = com.microsoft.bond.BondDataType.BT_STRUCT;
        SCHEMA.root.struct_def = 0;
        SCHEMA.root.element = null;
        SCHEMA.root.key = null;
        SCHEMA.root.bonded_type = false;

        STRUCT_DEF.metadata.name = "BasicTypes";
        STRUCT_DEF.metadata.qualified_name = "tests.BasicTypes";
        STRUCT_DEF.metadata.modifier = com.microsoft.bond.Modifier.Optional;
        
        // TODO: .base_def
        SCHEMA.structs.add(0, STRUCT_DEF);

        _bool_FIELD_DEF.metadata.name = "_bool";
        _bool_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _bool_FIELD_DEF.id = 0;
        _bool_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_BOOL;
        _bool_FIELD_DEF.type.struct_def = 0;
        _bool_FIELD_DEF.type.element = null;
        _bool_FIELD_DEF.type.key = null;
        _bool_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_bool_FIELD_DEF);

        _str_FIELD_DEF.metadata.name = "_str";
        _str_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _str_FIELD_DEF.id = 2;
        _str_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_STRING;
        _str_FIELD_DEF.type.struct_def = 0;
        _str_FIELD_DEF.type.element = null;
        _str_FIELD_DEF.type.key = null;
        _str_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_str_FIELD_DEF);

        _wstr_FIELD_DEF.metadata.name = "_wstr";
        _wstr_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _wstr_FIELD_DEF.id = 3;
        _wstr_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_WSTRING;
        _wstr_FIELD_DEF.type.struct_def = 0;
        _wstr_FIELD_DEF.type.element = null;
        _wstr_FIELD_DEF.type.key = null;
        _wstr_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_wstr_FIELD_DEF);

        _uint64_FIELD_DEF.metadata.name = "_uint64";
        _uint64_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _uint64_FIELD_DEF.id = 10;
        _uint64_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT64;
        _uint64_FIELD_DEF.type.struct_def = 0;
        _uint64_FIELD_DEF.type.element = null;
        _uint64_FIELD_DEF.type.key = null;
        _uint64_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_uint64_FIELD_DEF);

        _uint16_FIELD_DEF.metadata.name = "_uint16";
        _uint16_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _uint16_FIELD_DEF.id = 11;
        _uint16_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT16;
        _uint16_FIELD_DEF.type.struct_def = 0;
        _uint16_FIELD_DEF.type.element = null;
        _uint16_FIELD_DEF.type.key = null;
        _uint16_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_uint16_FIELD_DEF);

        _uint32_FIELD_DEF.metadata.name = "_uint32";
        _uint32_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _uint32_FIELD_DEF.id = 12;
        _uint32_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT32;
        _uint32_FIELD_DEF.type.struct_def = 0;
        _uint32_FIELD_DEF.type.element = null;
        _uint32_FIELD_DEF.type.key = null;
        _uint32_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_uint32_FIELD_DEF);

        _uint8_FIELD_DEF.metadata.name = "_uint8";
        _uint8_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _uint8_FIELD_DEF.id = 13;
        _uint8_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT8;
        _uint8_FIELD_DEF.type.struct_def = 0;
        _uint8_FIELD_DEF.type.element = null;
        _uint8_FIELD_DEF.type.key = null;
        _uint8_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_uint8_FIELD_DEF);

        _int8_FIELD_DEF.metadata.name = "_int8";
        _int8_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _int8_FIELD_DEF.id = 14;
        _int8_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT8;
        _int8_FIELD_DEF.type.struct_def = 0;
        _int8_FIELD_DEF.type.element = null;
        _int8_FIELD_DEF.type.key = null;
        _int8_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_int8_FIELD_DEF);

        _int16_FIELD_DEF.metadata.name = "_int16";
        _int16_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _int16_FIELD_DEF.id = 15;
        _int16_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT16;
        _int16_FIELD_DEF.type.struct_def = 0;
        _int16_FIELD_DEF.type.element = null;
        _int16_FIELD_DEF.type.key = null;
        _int16_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_int16_FIELD_DEF);

        _int32_FIELD_DEF.metadata.name = "_int32";
        _int32_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _int32_FIELD_DEF.id = 16;
        _int32_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        _int32_FIELD_DEF.type.struct_def = 0;
        _int32_FIELD_DEF.type.element = null;
        _int32_FIELD_DEF.type.key = null;
        _int32_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_int32_FIELD_DEF);

        _int64_FIELD_DEF.metadata.name = "_int64";
        _int64_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _int64_FIELD_DEF.id = 17;
        _int64_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT64;
        _int64_FIELD_DEF.type.struct_def = 0;
        _int64_FIELD_DEF.type.element = null;
        _int64_FIELD_DEF.type.key = null;
        _int64_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_int64_FIELD_DEF);

        _double_FIELD_DEF.metadata.name = "_double";
        _double_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _double_FIELD_DEF.id = 18;
        _double_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_DOUBLE;
        _double_FIELD_DEF.type.struct_def = 0;
        _double_FIELD_DEF.type.element = null;
        _double_FIELD_DEF.type.key = null;
        _double_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_double_FIELD_DEF);

        _float_FIELD_DEF.metadata.name = "_float";
        _float_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _float_FIELD_DEF.id = 20;
        _float_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_FLOAT;
        _float_FIELD_DEF.type.struct_def = 0;
        _float_FIELD_DEF.type.element = null;
        _float_FIELD_DEF.type.key = null;
        _float_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_float_FIELD_DEF);

        _blob_FIELD_DEF.metadata.name = "_blob";
        _blob_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        _blob_FIELD_DEF.id = 21;
        _blob_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_LIST;
        _blob_FIELD_DEF.type.struct_def = 0;
        _blob_FIELD_DEF.type.element = null;
        _blob_FIELD_DEF.type.key = null;
        _blob_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(_blob_FIELD_DEF);

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

    public boolean _bool = false;

    public String _str = "";

    public String _wstr = "";

    public long _uint64 = 0L;

    public short _uint16 = 0;

    public int _uint32 = 0;

    public byte _uint8 = 0;

    public byte _int8 = 0;

    public short _int16 = 0;

    public int _int32 = 0;

    public long _int64 = 0L;

    public double _double = 0.0;

    public float _float = 0.0f;

    public byte[] _blob = new byte[0];

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
        
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_BOOL, 0, _bool_FIELD_DEF.metadata);
        writer.writeBool(this._bool);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_STRING, 2, _str_FIELD_DEF.metadata);
        writer.writeString(this._str);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_WSTRING, 3, _wstr_FIELD_DEF.metadata);
        writer.writeWString(this._wstr);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT64, 10, _uint64_FIELD_DEF.metadata);
        writer.writeUInt64(this._uint64);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT16, 11, _uint16_FIELD_DEF.metadata);
        writer.writeUInt16(this._uint16);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT32, 12, _uint32_FIELD_DEF.metadata);
        writer.writeUInt32(this._uint32);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT8, 13, _uint8_FIELD_DEF.metadata);
        writer.writeUInt8(this._uint8);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT8, 14, _int8_FIELD_DEF.metadata);
        writer.writeInt8(this._int8);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT16, 15, _int16_FIELD_DEF.metadata);
        writer.writeInt16(this._int16);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 16, _int32_FIELD_DEF.metadata);
        writer.writeInt32(this._int32);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT64, 17, _int64_FIELD_DEF.metadata);
        writer.writeInt64(this._int64);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_DOUBLE, 18, _double_FIELD_DEF.metadata);
        writer.writeDouble(this._double);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_FLOAT, 20, _float_FIELD_DEF.metadata);
        writer.writeFloat(this._float);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_LIST, 21, _blob_FIELD_DEF.metadata);
        writer.writeContainerBegin(this._blob.length, com.microsoft.bond.BondDataType.BT_INT8);
        writer.writeBytes(this._blob);
        writer.writeContainerEnd();
        writer.writeFieldEnd();
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
        reader.readFieldBegin(this.__readFieldResult);
        this._bool = reader.readBool();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._str = reader.readString();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._wstr = reader.readWString();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._uint64 = reader.readUInt64();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._uint16 = reader.readUInt16();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._uint32 = reader.readUInt32();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._uint8 = reader.readUInt8();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._int8 = reader.readInt8();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._int16 = reader.readInt16();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._int32 = reader.readInt32();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._int64 = reader.readInt64();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._double = reader.readDouble();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this._float = reader.readFloat();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        reader.readListBegin(this.__readContainerResult);
        this._blob = reader.readBytes(__readContainerResult.count);
        reader.readContainerEnd();
        reader.readFieldEnd();
        
    }

    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }
}
