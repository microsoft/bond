
package tests;


@javax.annotation.Generated("gbc")
public final class EnumType1 implements com.microsoft.bond.BondEnum, java.lang.Comparable<EnumType1> {

    public static final class Values {
        private Values() {}

        public static final int EnumValue1 = 5;
        public static final int EnumValue2 = 10;
        public static final int EnumValue3 = -10;
        public static final int EnumValue4 = 42;
        public static final int Low = 1;
        public static final int EnumValue5 = -10;
        public static final int EnumValue6 = 4294967286;
        public static final int Int32Min = -2147483648;
        public static final int Int32Max = 2147483647;
        public static final int UInt32Min = 0;
        public static final int UInt32Max = 4294967295;
    }

    public static final EnumType1 EnumValue1 = new EnumType1(Values.EnumValue1, "EnumValue1");
    public static final EnumType1 EnumValue2 = new EnumType1(Values.EnumValue2, "EnumValue2");
    public static final EnumType1 EnumValue3 = new EnumType1(Values.EnumValue3, "EnumValue3");
    public static final EnumType1 EnumValue4 = new EnumType1(Values.EnumValue4, "EnumValue4");
    public static final EnumType1 Low = new EnumType1(Values.Low, "Low");
    public static final EnumType1 EnumValue5 = EnumValue3;
    public static final EnumType1 EnumValue6 = new EnumType1(Values.EnumValue6, "EnumValue6");
    public static final EnumType1 Int32Min = new EnumType1(Values.Int32Min, "Int32Min");
    public static final EnumType1 Int32Max = new EnumType1(Values.Int32Max, "Int32Max");
    public static final EnumType1 UInt32Min = new EnumType1(Values.UInt32Min, "UInt32Min");
    public static final EnumType1 UInt32Max = new EnumType1(Values.UInt32Max, "UInt32Max");

    public final int value;

    private final String label;

    private EnumType1(int value, String label) { this.value = value; this.label = label; }

    @Override
    public int getValue() { return this.value; }

    @Override
    public int compareTo(EnumType1 o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public boolean equals(Object other) { return (other instanceof EnumType1) && (this.value == ((EnumType1) other).value); }

    @Override
    public int hashCode() { return this.value; }

    @Override
    public String toString() { return this.label != null ? this.label : ("EnumType1(" + String.valueOf(this.value) + ")"); }

    public static EnumType1 get(int value) {
        switch (value) {
            case Values.EnumValue1: return EnumValue1;
            case Values.EnumValue2: return EnumValue2;
            case Values.EnumValue3: return EnumValue3;
            case Values.EnumValue4: return EnumValue4;
            case Values.Low: return Low;
            case Values.EnumValue6: return EnumValue6;
            case Values.Int32Min: return Int32Min;
            case Values.Int32Max: return Int32Max;
            case Values.UInt32Min: return UInt32Min;
            case Values.UInt32Max: return UInt32Max;
            default: return new EnumType1(value, null);
        }
    }
}

package tests;


@javax.annotation.Generated("gbc")
public class Foo implements com.microsoft.bond.BondSerializable {

    public static final com.microsoft.bond.SchemaDef SCHEMA = new com.microsoft.bond.SchemaDef();
    public static final com.microsoft.bond.StructDef STRUCT_DEF = new com.microsoft.bond.StructDef();
    private static final com.microsoft.bond.FieldDef m_bool_1_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_bool_2_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_bool_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_str_1_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_str_2_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int8_4_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int8_5_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int16_4_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int16_5_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int32_4_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int32_max_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int64_4_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_int64_max_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint8_2_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint8_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint16_2_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint16_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint32_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint32_max_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint64_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_uint64_max_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_double_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_double_4_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_double_5_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_float_3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_float_4_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_float_7_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum1_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum2_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum3_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum_int32min_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum_int32max_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum_uint32_min_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_enum_uint32_max_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_wstr_1_FIELD_DEF = new com.microsoft.bond.FieldDef();
    private static final com.microsoft.bond.FieldDef m_wstr_2_FIELD_DEF = new com.microsoft.bond.FieldDef();
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

        m_bool_1_FIELD_DEF.metadata.name = "m_bool_1";
        m_bool_1_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_bool_1_FIELD_DEF.id = 0;
        m_bool_1_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_BOOL;
        m_bool_1_FIELD_DEF.type.struct_def = 0;
        m_bool_1_FIELD_DEF.type.element = null;
        m_bool_1_FIELD_DEF.type.key = null;
        m_bool_1_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_bool_1_FIELD_DEF);

        m_bool_2_FIELD_DEF.metadata.name = "m_bool_2";
        m_bool_2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_bool_2_FIELD_DEF.id = 1;
        m_bool_2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_BOOL;
        m_bool_2_FIELD_DEF.type.struct_def = 0;
        m_bool_2_FIELD_DEF.type.element = null;
        m_bool_2_FIELD_DEF.type.key = null;
        m_bool_2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_bool_2_FIELD_DEF);

        m_bool_3_FIELD_DEF.metadata.name = "m_bool_3";
        m_bool_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_bool_3_FIELD_DEF.id = 2;
        m_bool_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_bool_3_FIELD_DEF.type.struct_def = 0;
        m_bool_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_bool_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_BOOL;
        m_bool_3_FIELD_DEF.type.element.struct_def = 0;
        m_bool_3_FIELD_DEF.type.element.element = null;
        m_bool_3_FIELD_DEF.type.element.key = null;
        m_bool_3_FIELD_DEF.type.element.bonded_type = false;
        m_bool_3_FIELD_DEF.type.key = null;
        m_bool_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_bool_3_FIELD_DEF);

        m_str_1_FIELD_DEF.metadata.name = "m_str_1";
        m_str_1_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_str_1_FIELD_DEF.id = 3;
        m_str_1_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_STRING;
        m_str_1_FIELD_DEF.type.struct_def = 0;
        m_str_1_FIELD_DEF.type.element = null;
        m_str_1_FIELD_DEF.type.key = null;
        m_str_1_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_str_1_FIELD_DEF);

        m_str_2_FIELD_DEF.metadata.name = "m_str_2";
        m_str_2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_str_2_FIELD_DEF.id = 4;
        m_str_2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_str_2_FIELD_DEF.type.struct_def = 0;
        m_str_2_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_str_2_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_STRING;
        m_str_2_FIELD_DEF.type.element.struct_def = 0;
        m_str_2_FIELD_DEF.type.element.element = null;
        m_str_2_FIELD_DEF.type.element.key = null;
        m_str_2_FIELD_DEF.type.element.bonded_type = false;
        m_str_2_FIELD_DEF.type.key = null;
        m_str_2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_str_2_FIELD_DEF);

        m_int8_4_FIELD_DEF.metadata.name = "m_int8_4";
        m_int8_4_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int8_4_FIELD_DEF.id = 5;
        m_int8_4_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT8;
        m_int8_4_FIELD_DEF.type.struct_def = 0;
        m_int8_4_FIELD_DEF.type.element = null;
        m_int8_4_FIELD_DEF.type.key = null;
        m_int8_4_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int8_4_FIELD_DEF);

        m_int8_5_FIELD_DEF.metadata.name = "m_int8_5";
        m_int8_5_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int8_5_FIELD_DEF.id = 6;
        m_int8_5_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_int8_5_FIELD_DEF.type.struct_def = 0;
        m_int8_5_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_int8_5_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_INT8;
        m_int8_5_FIELD_DEF.type.element.struct_def = 0;
        m_int8_5_FIELD_DEF.type.element.element = null;
        m_int8_5_FIELD_DEF.type.element.key = null;
        m_int8_5_FIELD_DEF.type.element.bonded_type = false;
        m_int8_5_FIELD_DEF.type.key = null;
        m_int8_5_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int8_5_FIELD_DEF);

        m_int16_4_FIELD_DEF.metadata.name = "m_int16_4";
        m_int16_4_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int16_4_FIELD_DEF.id = 7;
        m_int16_4_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT16;
        m_int16_4_FIELD_DEF.type.struct_def = 0;
        m_int16_4_FIELD_DEF.type.element = null;
        m_int16_4_FIELD_DEF.type.key = null;
        m_int16_4_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int16_4_FIELD_DEF);

        m_int16_5_FIELD_DEF.metadata.name = "m_int16_5";
        m_int16_5_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int16_5_FIELD_DEF.id = 8;
        m_int16_5_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_int16_5_FIELD_DEF.type.struct_def = 0;
        m_int16_5_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_int16_5_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_INT16;
        m_int16_5_FIELD_DEF.type.element.struct_def = 0;
        m_int16_5_FIELD_DEF.type.element.element = null;
        m_int16_5_FIELD_DEF.type.element.key = null;
        m_int16_5_FIELD_DEF.type.element.bonded_type = false;
        m_int16_5_FIELD_DEF.type.key = null;
        m_int16_5_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int16_5_FIELD_DEF);

        m_int32_4_FIELD_DEF.metadata.name = "m_int32_4";
        m_int32_4_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int32_4_FIELD_DEF.id = 9;
        m_int32_4_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_int32_4_FIELD_DEF.type.struct_def = 0;
        m_int32_4_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_int32_4_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_int32_4_FIELD_DEF.type.element.struct_def = 0;
        m_int32_4_FIELD_DEF.type.element.element = null;
        m_int32_4_FIELD_DEF.type.element.key = null;
        m_int32_4_FIELD_DEF.type.element.bonded_type = false;
        m_int32_4_FIELD_DEF.type.key = null;
        m_int32_4_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int32_4_FIELD_DEF);

        m_int32_max_FIELD_DEF.metadata.name = "m_int32_max";
        m_int32_max_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int32_max_FIELD_DEF.id = 10;
        m_int32_max_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_int32_max_FIELD_DEF.type.struct_def = 0;
        m_int32_max_FIELD_DEF.type.element = null;
        m_int32_max_FIELD_DEF.type.key = null;
        m_int32_max_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int32_max_FIELD_DEF);

        m_int64_4_FIELD_DEF.metadata.name = "m_int64_4";
        m_int64_4_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int64_4_FIELD_DEF.id = 11;
        m_int64_4_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_int64_4_FIELD_DEF.type.struct_def = 0;
        m_int64_4_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_int64_4_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_INT64;
        m_int64_4_FIELD_DEF.type.element.struct_def = 0;
        m_int64_4_FIELD_DEF.type.element.element = null;
        m_int64_4_FIELD_DEF.type.element.key = null;
        m_int64_4_FIELD_DEF.type.element.bonded_type = false;
        m_int64_4_FIELD_DEF.type.key = null;
        m_int64_4_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int64_4_FIELD_DEF);

        m_int64_max_FIELD_DEF.metadata.name = "m_int64_max";
        m_int64_max_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_int64_max_FIELD_DEF.id = 12;
        m_int64_max_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT64;
        m_int64_max_FIELD_DEF.type.struct_def = 0;
        m_int64_max_FIELD_DEF.type.element = null;
        m_int64_max_FIELD_DEF.type.key = null;
        m_int64_max_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_int64_max_FIELD_DEF);

        m_uint8_2_FIELD_DEF.metadata.name = "m_uint8_2";
        m_uint8_2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint8_2_FIELD_DEF.id = 13;
        m_uint8_2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT8;
        m_uint8_2_FIELD_DEF.type.struct_def = 0;
        m_uint8_2_FIELD_DEF.type.element = null;
        m_uint8_2_FIELD_DEF.type.key = null;
        m_uint8_2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint8_2_FIELD_DEF);

        m_uint8_3_FIELD_DEF.metadata.name = "m_uint8_3";
        m_uint8_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint8_3_FIELD_DEF.id = 14;
        m_uint8_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_uint8_3_FIELD_DEF.type.struct_def = 0;
        m_uint8_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_uint8_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_UINT8;
        m_uint8_3_FIELD_DEF.type.element.struct_def = 0;
        m_uint8_3_FIELD_DEF.type.element.element = null;
        m_uint8_3_FIELD_DEF.type.element.key = null;
        m_uint8_3_FIELD_DEF.type.element.bonded_type = false;
        m_uint8_3_FIELD_DEF.type.key = null;
        m_uint8_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint8_3_FIELD_DEF);

        m_uint16_2_FIELD_DEF.metadata.name = "m_uint16_2";
        m_uint16_2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint16_2_FIELD_DEF.id = 15;
        m_uint16_2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT16;
        m_uint16_2_FIELD_DEF.type.struct_def = 0;
        m_uint16_2_FIELD_DEF.type.element = null;
        m_uint16_2_FIELD_DEF.type.key = null;
        m_uint16_2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint16_2_FIELD_DEF);

        m_uint16_3_FIELD_DEF.metadata.name = "m_uint16_3";
        m_uint16_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint16_3_FIELD_DEF.id = 16;
        m_uint16_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_uint16_3_FIELD_DEF.type.struct_def = 0;
        m_uint16_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_uint16_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_UINT16;
        m_uint16_3_FIELD_DEF.type.element.struct_def = 0;
        m_uint16_3_FIELD_DEF.type.element.element = null;
        m_uint16_3_FIELD_DEF.type.element.key = null;
        m_uint16_3_FIELD_DEF.type.element.bonded_type = false;
        m_uint16_3_FIELD_DEF.type.key = null;
        m_uint16_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint16_3_FIELD_DEF);

        m_uint32_3_FIELD_DEF.metadata.name = "m_uint32_3";
        m_uint32_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint32_3_FIELD_DEF.id = 17;
        m_uint32_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_uint32_3_FIELD_DEF.type.struct_def = 0;
        m_uint32_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_uint32_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_UINT32;
        m_uint32_3_FIELD_DEF.type.element.struct_def = 0;
        m_uint32_3_FIELD_DEF.type.element.element = null;
        m_uint32_3_FIELD_DEF.type.element.key = null;
        m_uint32_3_FIELD_DEF.type.element.bonded_type = false;
        m_uint32_3_FIELD_DEF.type.key = null;
        m_uint32_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint32_3_FIELD_DEF);

        m_uint32_max_FIELD_DEF.metadata.name = "m_uint32_max";
        m_uint32_max_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint32_max_FIELD_DEF.id = 18;
        m_uint32_max_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT32;
        m_uint32_max_FIELD_DEF.type.struct_def = 0;
        m_uint32_max_FIELD_DEF.type.element = null;
        m_uint32_max_FIELD_DEF.type.key = null;
        m_uint32_max_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint32_max_FIELD_DEF);

        m_uint64_3_FIELD_DEF.metadata.name = "m_uint64_3";
        m_uint64_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint64_3_FIELD_DEF.id = 19;
        m_uint64_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_uint64_3_FIELD_DEF.type.struct_def = 0;
        m_uint64_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_uint64_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_UINT64;
        m_uint64_3_FIELD_DEF.type.element.struct_def = 0;
        m_uint64_3_FIELD_DEF.type.element.element = null;
        m_uint64_3_FIELD_DEF.type.element.key = null;
        m_uint64_3_FIELD_DEF.type.element.bonded_type = false;
        m_uint64_3_FIELD_DEF.type.key = null;
        m_uint64_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint64_3_FIELD_DEF);

        m_uint64_max_FIELD_DEF.metadata.name = "m_uint64_max";
        m_uint64_max_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_uint64_max_FIELD_DEF.id = 20;
        m_uint64_max_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UINT64;
        m_uint64_max_FIELD_DEF.type.struct_def = 0;
        m_uint64_max_FIELD_DEF.type.element = null;
        m_uint64_max_FIELD_DEF.type.key = null;
        m_uint64_max_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_uint64_max_FIELD_DEF);

        m_double_3_FIELD_DEF.metadata.name = "m_double_3";
        m_double_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_double_3_FIELD_DEF.id = 21;
        m_double_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_double_3_FIELD_DEF.type.struct_def = 0;
        m_double_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_double_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_DOUBLE;
        m_double_3_FIELD_DEF.type.element.struct_def = 0;
        m_double_3_FIELD_DEF.type.element.element = null;
        m_double_3_FIELD_DEF.type.element.key = null;
        m_double_3_FIELD_DEF.type.element.bonded_type = false;
        m_double_3_FIELD_DEF.type.key = null;
        m_double_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_double_3_FIELD_DEF);

        m_double_4_FIELD_DEF.metadata.name = "m_double_4";
        m_double_4_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_double_4_FIELD_DEF.id = 22;
        m_double_4_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_DOUBLE;
        m_double_4_FIELD_DEF.type.struct_def = 0;
        m_double_4_FIELD_DEF.type.element = null;
        m_double_4_FIELD_DEF.type.key = null;
        m_double_4_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_double_4_FIELD_DEF);

        m_double_5_FIELD_DEF.metadata.name = "m_double_5";
        m_double_5_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_double_5_FIELD_DEF.id = 23;
        m_double_5_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_DOUBLE;
        m_double_5_FIELD_DEF.type.struct_def = 0;
        m_double_5_FIELD_DEF.type.element = null;
        m_double_5_FIELD_DEF.type.key = null;
        m_double_5_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_double_5_FIELD_DEF);

        m_float_3_FIELD_DEF.metadata.name = "m_float_3";
        m_float_3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_float_3_FIELD_DEF.id = 24;
        m_float_3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_float_3_FIELD_DEF.type.struct_def = 0;
        m_float_3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_float_3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_FLOAT;
        m_float_3_FIELD_DEF.type.element.struct_def = 0;
        m_float_3_FIELD_DEF.type.element.element = null;
        m_float_3_FIELD_DEF.type.element.key = null;
        m_float_3_FIELD_DEF.type.element.bonded_type = false;
        m_float_3_FIELD_DEF.type.key = null;
        m_float_3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_float_3_FIELD_DEF);

        m_float_4_FIELD_DEF.metadata.name = "m_float_4";
        m_float_4_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_float_4_FIELD_DEF.id = 25;
        m_float_4_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_FLOAT;
        m_float_4_FIELD_DEF.type.struct_def = 0;
        m_float_4_FIELD_DEF.type.element = null;
        m_float_4_FIELD_DEF.type.key = null;
        m_float_4_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_float_4_FIELD_DEF);

        m_float_7_FIELD_DEF.metadata.name = "m_float_7";
        m_float_7_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_float_7_FIELD_DEF.id = 26;
        m_float_7_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_FLOAT;
        m_float_7_FIELD_DEF.type.struct_def = 0;
        m_float_7_FIELD_DEF.type.element = null;
        m_float_7_FIELD_DEF.type.key = null;
        m_float_7_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_float_7_FIELD_DEF);

        m_enum1_FIELD_DEF.metadata.name = "m_enum1";
        m_enum1_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum1_FIELD_DEF.id = 27;
        m_enum1_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum1_FIELD_DEF.type.struct_def = 0;
        m_enum1_FIELD_DEF.type.element = null;
        m_enum1_FIELD_DEF.type.key = null;
        m_enum1_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum1_FIELD_DEF);

        m_enum2_FIELD_DEF.metadata.name = "m_enum2";
        m_enum2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum2_FIELD_DEF.id = 28;
        m_enum2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum2_FIELD_DEF.type.struct_def = 0;
        m_enum2_FIELD_DEF.type.element = null;
        m_enum2_FIELD_DEF.type.key = null;
        m_enum2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum2_FIELD_DEF);

        m_enum3_FIELD_DEF.metadata.name = "m_enum3";
        m_enum3_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum3_FIELD_DEF.id = 29;
        m_enum3_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_enum3_FIELD_DEF.type.struct_def = 0;
        m_enum3_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_enum3_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum3_FIELD_DEF.type.element.struct_def = 0;
        m_enum3_FIELD_DEF.type.element.element = null;
        m_enum3_FIELD_DEF.type.element.key = null;
        m_enum3_FIELD_DEF.type.element.bonded_type = false;
        m_enum3_FIELD_DEF.type.key = null;
        m_enum3_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum3_FIELD_DEF);

        m_enum_int32min_FIELD_DEF.metadata.name = "m_enum_int32min";
        m_enum_int32min_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum_int32min_FIELD_DEF.id = 30;
        m_enum_int32min_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum_int32min_FIELD_DEF.type.struct_def = 0;
        m_enum_int32min_FIELD_DEF.type.element = null;
        m_enum_int32min_FIELD_DEF.type.key = null;
        m_enum_int32min_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum_int32min_FIELD_DEF);

        m_enum_int32max_FIELD_DEF.metadata.name = "m_enum_int32max";
        m_enum_int32max_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum_int32max_FIELD_DEF.id = 31;
        m_enum_int32max_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum_int32max_FIELD_DEF.type.struct_def = 0;
        m_enum_int32max_FIELD_DEF.type.element = null;
        m_enum_int32max_FIELD_DEF.type.key = null;
        m_enum_int32max_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum_int32max_FIELD_DEF);

        m_enum_uint32_min_FIELD_DEF.metadata.name = "m_enum_uint32_min";
        m_enum_uint32_min_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum_uint32_min_FIELD_DEF.id = 32;
        m_enum_uint32_min_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum_uint32_min_FIELD_DEF.type.struct_def = 0;
        m_enum_uint32_min_FIELD_DEF.type.element = null;
        m_enum_uint32_min_FIELD_DEF.type.key = null;
        m_enum_uint32_min_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum_uint32_min_FIELD_DEF);

        m_enum_uint32_max_FIELD_DEF.metadata.name = "m_enum_uint32_max";
        m_enum_uint32_max_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_enum_uint32_max_FIELD_DEF.id = 33;
        m_enum_uint32_max_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_INT32;
        m_enum_uint32_max_FIELD_DEF.type.struct_def = 0;
        m_enum_uint32_max_FIELD_DEF.type.element = null;
        m_enum_uint32_max_FIELD_DEF.type.key = null;
        m_enum_uint32_max_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_enum_uint32_max_FIELD_DEF);

        m_wstr_1_FIELD_DEF.metadata.name = "m_wstr_1";
        m_wstr_1_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_wstr_1_FIELD_DEF.id = 34;
        m_wstr_1_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_WSTRING;
        m_wstr_1_FIELD_DEF.type.struct_def = 0;
        m_wstr_1_FIELD_DEF.type.element = null;
        m_wstr_1_FIELD_DEF.type.key = null;
        m_wstr_1_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_wstr_1_FIELD_DEF);

        m_wstr_2_FIELD_DEF.metadata.name = "m_wstr_2";
        m_wstr_2_FIELD_DEF.metadata.qualified_name = "";
        // TODO: .metadata.qualifier
        
        m_wstr_2_FIELD_DEF.id = 35;
        m_wstr_2_FIELD_DEF.type.id = com.microsoft.bond.BondDataType.BT_UNAVAILABLE;
        m_wstr_2_FIELD_DEF.type.struct_def = 0;
        m_wstr_2_FIELD_DEF.type.element = new com.microsoft.bond.TypeDef();
        m_wstr_2_FIELD_DEF.type.element.id = com.microsoft.bond.BondDataType.BT_WSTRING;
        m_wstr_2_FIELD_DEF.type.element.struct_def = 0;
        m_wstr_2_FIELD_DEF.type.element.element = null;
        m_wstr_2_FIELD_DEF.type.element.key = null;
        m_wstr_2_FIELD_DEF.type.element.bonded_type = false;
        m_wstr_2_FIELD_DEF.type.key = null;
        m_wstr_2_FIELD_DEF.type.bonded_type = false;
        STRUCT_DEF.fields.add(m_wstr_2_FIELD_DEF);

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

    public boolean m_bool_1 = true;

    public boolean m_bool_2 = false;

    public Boolean m_bool_3 = null;

    public String m_str_1 = "default string value";

    public String m_str_2 = null;

    public byte m_int8_4 = -127;

    public Byte m_int8_5 = null;

    public short m_int16_4 = -32767;

    public Short m_int16_5 = null;

    public Integer m_int32_4 = null;

    public int m_int32_max = 2147483647;

    public Long m_int64_4 = null;

    public long m_int64_max = 9223372036854775807L;

    public byte m_uint8_2 = 255;

    public Byte m_uint8_3 = null;

    public short m_uint16_2 = 65535;

    public Short m_uint16_3 = null;

    public Integer m_uint32_3 = null;

    public int m_uint32_max = 4294967295L;

    public Long m_uint64_3 = null;

    public long m_uint64_max = 18446744073709551615L;

    public Double m_double_3 = null;

    public double m_double_4 = -123.456789;

    public double m_double_5 = -0.0;

    public Float m_float_3 = null;

    public float m_float_4 = 2.71828183f;

    public float m_float_7 = 0.0f;

    public EnumType1 m_enum1 = EnumType1.EnumValue1;

    public EnumType1 m_enum2 = EnumType1.EnumValue3;

    public EnumType1 m_enum3 = null;

    public EnumType1 m_enum_int32min = EnumType1.Int32Min;

    public EnumType1 m_enum_int32max = EnumType1.Int32Max;

    public EnumType1 m_enum_uint32_min = EnumType1.UInt32Min;

    public EnumType1 m_enum_uint32_max = EnumType1.UInt32Max;

    public String m_wstr_1 = "default wstring value";

    public String m_wstr_2 = null;

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
        
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_BOOL, 0, m_bool_1_FIELD_DEF.metadata);
        writer.writeBool(this.m_bool_1);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_BOOL, 1, m_bool_2_FIELD_DEF.metadata);
        writer.writeBool(this.m_bool_2);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 2, m_bool_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_STRING, 3, m_str_1_FIELD_DEF.metadata);
        writer.writeString(this.m_str_1);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 4, m_str_2_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT8, 5, m_int8_4_FIELD_DEF.metadata);
        writer.writeInt8(this.m_int8_4);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 6, m_int8_5_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT16, 7, m_int16_4_FIELD_DEF.metadata);
        writer.writeInt16(this.m_int16_4);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 8, m_int16_5_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 9, m_int32_4_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 10, m_int32_max_FIELD_DEF.metadata);
        writer.writeInt32(this.m_int32_max);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 11, m_int64_4_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT64, 12, m_int64_max_FIELD_DEF.metadata);
        writer.writeInt64(this.m_int64_max);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT8, 13, m_uint8_2_FIELD_DEF.metadata);
        writer.writeUInt8(this.m_uint8_2);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 14, m_uint8_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT16, 15, m_uint16_2_FIELD_DEF.metadata);
        writer.writeUInt16(this.m_uint16_2);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 16, m_uint16_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 17, m_uint32_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT32, 18, m_uint32_max_FIELD_DEF.metadata);
        writer.writeUInt32(this.m_uint32_max);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 19, m_uint64_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UINT64, 20, m_uint64_max_FIELD_DEF.metadata);
        writer.writeUInt64(this.m_uint64_max);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 21, m_double_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_DOUBLE, 22, m_double_4_FIELD_DEF.metadata);
        writer.writeDouble(this.m_double_4);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_DOUBLE, 23, m_double_5_FIELD_DEF.metadata);
        writer.writeDouble(this.m_double_5);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 24, m_float_3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_FLOAT, 25, m_float_4_FIELD_DEF.metadata);
        writer.writeFloat(this.m_float_4);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_FLOAT, 26, m_float_7_FIELD_DEF.metadata);
        writer.writeFloat(this.m_float_7);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 27, m_enum1_FIELD_DEF.metadata);
        writer.writeInt32(this.m_enum1.value);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 28, m_enum2_FIELD_DEF.metadata);
        writer.writeInt32(this.m_enum2.value);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 29, m_enum3_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 30, m_enum_int32min_FIELD_DEF.metadata);
        writer.writeInt32(this.m_enum_int32min.value);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 31, m_enum_int32max_FIELD_DEF.metadata);
        writer.writeInt32(this.m_enum_int32max.value);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 32, m_enum_uint32_min_FIELD_DEF.metadata);
        writer.writeInt32(this.m_enum_uint32_min.value);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_INT32, 33, m_enum_uint32_max_FIELD_DEF.metadata);
        writer.writeInt32(this.m_enum_uint32_max.value);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_WSTRING, 34, m_wstr_1_FIELD_DEF.metadata);
        writer.writeWString(this.m_wstr_1);
        writer.writeFieldEnd();
        
        writer.writeFieldBegin(com.microsoft.bond.BondDataType.BT_UNAVAILABLE, 35, m_wstr_2_FIELD_DEF.metadata);
        // FIXME: Not implemented.
        writer.writeFieldEnd();
        
    }

    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_bool_1 = reader.readBool();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_bool_2 = reader.readBool();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_str_1 = reader.readString();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_int8_4 = reader.readInt8();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_int16_4 = reader.readInt16();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_int32_max = reader.readInt32();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_int64_max = reader.readInt64();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_uint8_2 = reader.readUInt8();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_uint16_2 = reader.readUInt16();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_uint32_max = reader.readUInt32();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_uint64_max = reader.readUInt64();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_double_4 = reader.readDouble();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_double_5 = reader.readDouble();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_float_4 = reader.readFloat();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_float_7 = reader.readFloat();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_enum1 = EnumType1.get(reader.readInt32());
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_enum2 = EnumType1.get(reader.readInt32());
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_enum_int32min = EnumType1.get(reader.readInt32());
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_enum_int32max = EnumType1.get(reader.readInt32());
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_enum_uint32_min = EnumType1.get(reader.readInt32());
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_enum_uint32_max = EnumType1.get(reader.readInt32());
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        this.m_wstr_1 = reader.readWString();
        reader.readFieldEnd();
        
        reader.readFieldBegin(this.__readFieldResult);
        // FIXME: Not implemented.
        reader.readFieldEnd();
        
    }

    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }
}
