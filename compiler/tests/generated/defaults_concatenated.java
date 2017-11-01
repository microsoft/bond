
package tests;


@javax.annotation.Generated("gbc")
public final class EnumType1 implements org.bondlib.BondEnum<EnumType1> {

    public static final class Values {
        private Values() {}

        public static final int EnumValue1 = 5;
        public static final int EnumValue2 = 10;
        public static final int EnumValue3 = -10;
        public static final int EnumValue4 = 42;
        public static final int Low = 1;
        public static final int EnumValue5 = -10;
        public static final int EnumValue6 = -10;
        public static final int Int32Min = -2147483648;
        public static final int Int32Max = 2147483647;
        public static final int UInt32Min = 0;
        public static final int UInt32Max = -1;
        public static final int HexNeg = -255;
        public static final int OctNeg = -83;
    }

    private static final class EnumBondTypeImpl extends org.bondlib.EnumBondType<EnumType1> {

        @Override
        public java.lang.Class<EnumType1> getValueClass() { return EnumType1.class; }

        @Override
        public final EnumType1 getEnumValue(int value) { return get(value); }
    }

    public static final org.bondlib.EnumBondType<EnumType1> BOND_TYPE = new EnumBondTypeImpl();

    public static final EnumType1 EnumValue1 = new EnumType1(Values.EnumValue1, "EnumValue1");
    public static final EnumType1 EnumValue2 = new EnumType1(Values.EnumValue2, "EnumValue2");
    public static final EnumType1 EnumValue3 = new EnumType1(Values.EnumValue3, "EnumValue3");
    public static final EnumType1 EnumValue4 = new EnumType1(Values.EnumValue4, "EnumValue4");
    public static final EnumType1 Low = new EnumType1(Values.Low, "Low");
    public static final EnumType1 EnumValue5 = EnumValue3;
    public static final EnumType1 EnumValue6 = EnumValue3;
    public static final EnumType1 Int32Min = new EnumType1(Values.Int32Min, "Int32Min");
    public static final EnumType1 Int32Max = new EnumType1(Values.Int32Max, "Int32Max");
    public static final EnumType1 UInt32Min = new EnumType1(Values.UInt32Min, "UInt32Min");
    public static final EnumType1 UInt32Max = new EnumType1(Values.UInt32Max, "UInt32Max");
    public static final EnumType1 HexNeg = new EnumType1(Values.HexNeg, "HexNeg");
    public static final EnumType1 OctNeg = new EnumType1(Values.OctNeg, "OctNeg");

    public final int value;

    private final java.lang.String label;

    private EnumType1(int value, java.lang.String label) { this.value = value; this.label = label; }

    @Override
    public final int getValue() { return this.value; }

    @Override
    public final java.lang.String getLabel() { return this.label; }

    @Override
    public final org.bondlib.EnumBondType<EnumType1> getBondType() { return BOND_TYPE; }

    @Override
    public final int compareTo(EnumType1 o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public final boolean equals(java.lang.Object other) { return (other instanceof EnumType1) && (this.value == ((EnumType1) other).value); }

    @Override
    public final int hashCode() { return this.value; }

    @Override
    public final java.lang.String toString() { return this.label != null ? this.label : ("EnumType1(" + java.lang.String.valueOf(this.value) + ")"); }

    public static EnumType1 get(int value) {
        switch (value) {
            case Values.EnumValue1: return EnumValue1;
            case Values.EnumValue2: return EnumValue2;
            case Values.EnumValue3: return EnumValue3;
            case Values.EnumValue4: return EnumValue4;
            case Values.Low: return Low;
            case Values.Int32Min: return Int32Min;
            case Values.Int32Max: return Int32Max;
            case Values.UInt32Min: return UInt32Min;
            case Values.UInt32Max: return UInt32Max;
            case Values.HexNeg: return HexNeg;
            case Values.OctNeg: return OctNeg;
            default: return new EnumType1(value, null);
        }
    }

    public static EnumType1 valueOf(java.lang.String str) {
        if (str == null) {
            throw new java.lang.IllegalArgumentException("Argument 'str' must not be null.");
        } else if (str.equals("EnumValue1")) {
            return EnumValue1;
        } else if (str.equals("EnumValue2")) {
            return EnumValue2;
        } else if (str.equals("EnumValue3")) {
            return EnumValue3;
        } else if (str.equals("EnumValue4")) {
            return EnumValue4;
        } else if (str.equals("Low")) {
            return Low;
        } else if (str.equals("EnumValue5")) {
            return EnumValue5;
        } else if (str.equals("EnumValue6")) {
            return EnumValue6;
        } else if (str.equals("Int32Min")) {
            return Int32Min;
        } else if (str.equals("Int32Max")) {
            return Int32Max;
        } else if (str.equals("UInt32Min")) {
            return UInt32Min;
        } else if (str.equals("UInt32Max")) {
            return UInt32Max;
        } else if (str.equals("HexNeg")) {
            return HexNeg;
        } else if (str.equals("OctNeg")) {
            return OctNeg;
        } else {
            throw new java.lang.IllegalArgumentException("Invalid 'EnumType1' enum value: '" + str + "'.");
        }
    }
}

package tests;

@javax.annotation.Generated("gbc")
public class Foo implements org.bondlib.BondSerializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<Foo> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<Foo> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<Foo> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(Foo.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.BoolStructField m_bool_1;

        private org.bondlib.StructBondType.BoolStructField m_bool_2;

        private org.bondlib.StructBondType.SomethingBoolStructField m_bool_3;

        private org.bondlib.StructBondType.StringStructField m_str_1;

        private org.bondlib.StructBondType.SomethingStringStructField m_str_2;

        private org.bondlib.StructBondType.Int8StructField m_int8_4;

        private org.bondlib.StructBondType.SomethingInt8StructField m_int8_5;

        private org.bondlib.StructBondType.Int16StructField m_int16_4;

        private org.bondlib.StructBondType.SomethingInt16StructField m_int16_5;

        private org.bondlib.StructBondType.SomethingInt32StructField m_int32_4;

        private org.bondlib.StructBondType.Int32StructField m_int32_max;

        private org.bondlib.StructBondType.SomethingInt64StructField m_int64_4;

        private org.bondlib.StructBondType.Int64StructField m_int64_max;

        private org.bondlib.StructBondType.UInt8StructField m_uint8_2;

        private org.bondlib.StructBondType.SomethingUInt8StructField m_uint8_3;

        private org.bondlib.StructBondType.UInt16StructField m_uint16_2;

        private org.bondlib.StructBondType.SomethingUInt16StructField m_uint16_3;

        private org.bondlib.StructBondType.SomethingUInt32StructField m_uint32_3;

        private org.bondlib.StructBondType.UInt32StructField m_uint32_max;

        private org.bondlib.StructBondType.SomethingUInt64StructField m_uint64_3;

        private org.bondlib.StructBondType.UInt64StructField m_uint64_max;

        private org.bondlib.StructBondType.SomethingDoubleStructField m_double_3;

        private org.bondlib.StructBondType.DoubleStructField m_double_4;

        private org.bondlib.StructBondType.DoubleStructField m_double_5;

        private org.bondlib.StructBondType.SomethingFloatStructField m_float_3;

        private org.bondlib.StructBondType.FloatStructField m_float_4;

        private org.bondlib.StructBondType.FloatStructField m_float_7;

        private org.bondlib.StructBondType.EnumStructField<tests.EnumType1> m_enum1;

        private org.bondlib.StructBondType.EnumStructField<tests.EnumType1> m_enum2;

        private org.bondlib.StructBondType.SomethingEnumStructField<tests.EnumType1> m_enum3;

        private org.bondlib.StructBondType.EnumStructField<tests.EnumType1> m_enum_int32min;

        private org.bondlib.StructBondType.EnumStructField<tests.EnumType1> m_enum_int32max;

        private org.bondlib.StructBondType.EnumStructField<tests.EnumType1> m_enum_uint32_min;

        private org.bondlib.StructBondType.EnumStructField<tests.EnumType1> m_enum_uint32_max;

        private org.bondlib.StructBondType.WStringStructField m_wstr_1;

        private org.bondlib.StructBondType.SomethingWStringStructField m_wstr_2;

        private org.bondlib.StructBondType.Int64StructField m_int64_neg_hex;

        private org.bondlib.StructBondType.Int64StructField m_int64_neg_oct;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.m_bool_1 = new org.bondlib.StructBondType.BoolStructField(this, 0, "m_bool_1", org.bondlib.Modifier.Optional, true);
            this.m_bool_2 = new org.bondlib.StructBondType.BoolStructField(this, 1, "m_bool_2", org.bondlib.Modifier.Optional, false);
            this.m_bool_3 = new org.bondlib.StructBondType.SomethingBoolStructField(this, 2, "m_bool_3", org.bondlib.Modifier.Optional);
            this.m_str_1 = new org.bondlib.StructBondType.StringStructField(this, 3, "m_str_1", org.bondlib.Modifier.Optional, "default string value");
            this.m_str_2 = new org.bondlib.StructBondType.SomethingStringStructField(this, 4, "m_str_2", org.bondlib.Modifier.Optional);
            this.m_int8_4 = new org.bondlib.StructBondType.Int8StructField(this, 5, "m_int8_4", org.bondlib.Modifier.Optional, (byte)-127);
            this.m_int8_5 = new org.bondlib.StructBondType.SomethingInt8StructField(this, 6, "m_int8_5", org.bondlib.Modifier.Optional);
            this.m_int16_4 = new org.bondlib.StructBondType.Int16StructField(this, 7, "m_int16_4", org.bondlib.Modifier.Optional, (short)-32767);
            this.m_int16_5 = new org.bondlib.StructBondType.SomethingInt16StructField(this, 8, "m_int16_5", org.bondlib.Modifier.Optional);
            this.m_int32_4 = new org.bondlib.StructBondType.SomethingInt32StructField(this, 9, "m_int32_4", org.bondlib.Modifier.Optional);
            this.m_int32_max = new org.bondlib.StructBondType.Int32StructField(this, 10, "m_int32_max", org.bondlib.Modifier.Optional, 2147483647);
            this.m_int64_4 = new org.bondlib.StructBondType.SomethingInt64StructField(this, 11, "m_int64_4", org.bondlib.Modifier.Optional);
            this.m_int64_max = new org.bondlib.StructBondType.Int64StructField(this, 12, "m_int64_max", org.bondlib.Modifier.Optional, 9223372036854775807L);
            this.m_uint8_2 = new org.bondlib.StructBondType.UInt8StructField(this, 13, "m_uint8_2", org.bondlib.Modifier.Optional, (byte)-1);
            this.m_uint8_3 = new org.bondlib.StructBondType.SomethingUInt8StructField(this, 14, "m_uint8_3", org.bondlib.Modifier.Optional);
            this.m_uint16_2 = new org.bondlib.StructBondType.UInt16StructField(this, 15, "m_uint16_2", org.bondlib.Modifier.Optional, (short)-1);
            this.m_uint16_3 = new org.bondlib.StructBondType.SomethingUInt16StructField(this, 16, "m_uint16_3", org.bondlib.Modifier.Optional);
            this.m_uint32_3 = new org.bondlib.StructBondType.SomethingUInt32StructField(this, 17, "m_uint32_3", org.bondlib.Modifier.Optional);
            this.m_uint32_max = new org.bondlib.StructBondType.UInt32StructField(this, 18, "m_uint32_max", org.bondlib.Modifier.Optional, -1);
            this.m_uint64_3 = new org.bondlib.StructBondType.SomethingUInt64StructField(this, 19, "m_uint64_3", org.bondlib.Modifier.Optional);
            this.m_uint64_max = new org.bondlib.StructBondType.UInt64StructField(this, 20, "m_uint64_max", org.bondlib.Modifier.Optional, -1L);
            this.m_double_3 = new org.bondlib.StructBondType.SomethingDoubleStructField(this, 21, "m_double_3", org.bondlib.Modifier.Optional);
            this.m_double_4 = new org.bondlib.StructBondType.DoubleStructField(this, 22, "m_double_4", org.bondlib.Modifier.Optional, -123.456789D);
            this.m_double_5 = new org.bondlib.StructBondType.DoubleStructField(this, 23, "m_double_5", org.bondlib.Modifier.Optional, -0.0D);
            this.m_float_3 = new org.bondlib.StructBondType.SomethingFloatStructField(this, 24, "m_float_3", org.bondlib.Modifier.Optional);
            this.m_float_4 = new org.bondlib.StructBondType.FloatStructField(this, 25, "m_float_4", org.bondlib.Modifier.Optional, 2.71828183F);
            this.m_float_7 = new org.bondlib.StructBondType.FloatStructField(this, 26, "m_float_7", org.bondlib.Modifier.Optional, 0.0F);
            this.m_enum1 = new org.bondlib.StructBondType.EnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 27, "m_enum1", org.bondlib.Modifier.Optional, tests.EnumType1.EnumValue1);
            this.m_enum2 = new org.bondlib.StructBondType.EnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 28, "m_enum2", org.bondlib.Modifier.Optional, tests.EnumType1.EnumValue3);
            this.m_enum3 = new org.bondlib.StructBondType.SomethingEnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 29, "m_enum3", org.bondlib.Modifier.Optional);
            this.m_enum_int32min = new org.bondlib.StructBondType.EnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 30, "m_enum_int32min", org.bondlib.Modifier.Optional, tests.EnumType1.Int32Min);
            this.m_enum_int32max = new org.bondlib.StructBondType.EnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 31, "m_enum_int32max", org.bondlib.Modifier.Optional, tests.EnumType1.Int32Max);
            this.m_enum_uint32_min = new org.bondlib.StructBondType.EnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 32, "m_enum_uint32_min", org.bondlib.Modifier.Optional, tests.EnumType1.UInt32Min);
            this.m_enum_uint32_max = new org.bondlib.StructBondType.EnumStructField<tests.EnumType1>(this, tests.EnumType1.BOND_TYPE, 33, "m_enum_uint32_max", org.bondlib.Modifier.Optional, tests.EnumType1.UInt32Max);
            this.m_wstr_1 = new org.bondlib.StructBondType.WStringStructField(this, 34, "m_wstr_1", org.bondlib.Modifier.Optional, "default wstring value");
            this.m_wstr_2 = new org.bondlib.StructBondType.SomethingWStringStructField(this, 35, "m_wstr_2", org.bondlib.Modifier.Optional);
            this.m_int64_neg_hex = new org.bondlib.StructBondType.Int64StructField(this, 36, "m_int64_neg_hex", org.bondlib.Modifier.Optional, -4095L);
            this.m_int64_neg_oct = new org.bondlib.StructBondType.Int64StructField(this, 37, "m_int64_neg_oct", org.bondlib.Modifier.Optional, -83L);
            super.initializeBaseAndFields(null, this.m_bool_1, this.m_bool_2, this.m_bool_3, this.m_str_1, this.m_str_2, this.m_int8_4, this.m_int8_5, this.m_int16_4, this.m_int16_5, this.m_int32_4, this.m_int32_max, this.m_int64_4, this.m_int64_max, this.m_uint8_2, this.m_uint8_3, this.m_uint16_2, this.m_uint16_3, this.m_uint32_3, this.m_uint32_max, this.m_uint64_3, this.m_uint64_max, this.m_double_3, this.m_double_4, this.m_double_5, this.m_float_3, this.m_float_4, this.m_float_7, this.m_enum1, this.m_enum2, this.m_enum3, this.m_enum_int32min, this.m_enum_int32max, this.m_enum_uint32_min, this.m_enum_uint32_max, this.m_wstr_1, this.m_wstr_2, this.m_int64_neg_hex, this.m_int64_neg_oct);
        }

        @Override
        public final java.lang.String getName() {
            return "Foo";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "tests.Foo";
        }

        @Override
        public final java.lang.Class<Foo> getValueClass() {
            return (java.lang.Class<Foo>) (java.lang.Class) Foo.class;
        }

        @Override
        public final Foo newInstance() {
            return new Foo();
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, Foo value) throws java.io.IOException {
            this.m_bool_1.serialize(context, value.m_bool_1);
            this.m_bool_2.serialize(context, value.m_bool_2);
            this.m_bool_3.serialize(context, value.m_bool_3);
            this.m_str_1.serialize(context, value.m_str_1);
            this.m_str_2.serialize(context, value.m_str_2);
            this.m_int8_4.serialize(context, value.m_int8_4);
            this.m_int8_5.serialize(context, value.m_int8_5);
            this.m_int16_4.serialize(context, value.m_int16_4);
            this.m_int16_5.serialize(context, value.m_int16_5);
            this.m_int32_4.serialize(context, value.m_int32_4);
            this.m_int32_max.serialize(context, value.m_int32_max);
            this.m_int64_4.serialize(context, value.m_int64_4);
            this.m_int64_max.serialize(context, value.m_int64_max);
            this.m_uint8_2.serialize(context, value.m_uint8_2);
            this.m_uint8_3.serialize(context, value.m_uint8_3);
            this.m_uint16_2.serialize(context, value.m_uint16_2);
            this.m_uint16_3.serialize(context, value.m_uint16_3);
            this.m_uint32_3.serialize(context, value.m_uint32_3);
            this.m_uint32_max.serialize(context, value.m_uint32_max);
            this.m_uint64_3.serialize(context, value.m_uint64_3);
            this.m_uint64_max.serialize(context, value.m_uint64_max);
            this.m_double_3.serialize(context, value.m_double_3);
            this.m_double_4.serialize(context, value.m_double_4);
            this.m_double_5.serialize(context, value.m_double_5);
            this.m_float_3.serialize(context, value.m_float_3);
            this.m_float_4.serialize(context, value.m_float_4);
            this.m_float_7.serialize(context, value.m_float_7);
            this.m_enum1.serialize(context, value.m_enum1);
            this.m_enum2.serialize(context, value.m_enum2);
            this.m_enum3.serialize(context, value.m_enum3);
            this.m_enum_int32min.serialize(context, value.m_enum_int32min);
            this.m_enum_int32max.serialize(context, value.m_enum_int32max);
            this.m_enum_uint32_min.serialize(context, value.m_enum_uint32_min);
            this.m_enum_uint32_max.serialize(context, value.m_enum_uint32_max);
            this.m_wstr_1.serialize(context, value.m_wstr_1);
            this.m_wstr_2.serialize(context, value.m_wstr_2);
            this.m_int64_neg_hex.serialize(context, value.m_int64_neg_hex);
            this.m_int64_neg_oct.serialize(context, value.m_int64_neg_oct);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
            boolean __has_m_bool_1 = false;
            boolean __has_m_bool_2 = false;
            boolean __has_m_bool_3 = false;
            boolean __has_m_str_1 = false;
            boolean __has_m_str_2 = false;
            boolean __has_m_int8_4 = false;
            boolean __has_m_int8_5 = false;
            boolean __has_m_int16_4 = false;
            boolean __has_m_int16_5 = false;
            boolean __has_m_int32_4 = false;
            boolean __has_m_int32_max = false;
            boolean __has_m_int64_4 = false;
            boolean __has_m_int64_max = false;
            boolean __has_m_uint8_2 = false;
            boolean __has_m_uint8_3 = false;
            boolean __has_m_uint16_2 = false;
            boolean __has_m_uint16_3 = false;
            boolean __has_m_uint32_3 = false;
            boolean __has_m_uint32_max = false;
            boolean __has_m_uint64_3 = false;
            boolean __has_m_uint64_max = false;
            boolean __has_m_double_3 = false;
            boolean __has_m_double_4 = false;
            boolean __has_m_double_5 = false;
            boolean __has_m_float_3 = false;
            boolean __has_m_float_4 = false;
            boolean __has_m_float_7 = false;
            boolean __has_m_enum1 = false;
            boolean __has_m_enum2 = false;
            boolean __has_m_enum3 = false;
            boolean __has_m_enum_int32min = false;
            boolean __has_m_enum_int32max = false;
            boolean __has_m_enum_uint32_min = false;
            boolean __has_m_enum_uint32_max = false;
            boolean __has_m_wstr_1 = false;
            boolean __has_m_wstr_2 = false;
            boolean __has_m_int64_neg_hex = false;
            boolean __has_m_int64_neg_oct = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.m_bool_1 = this.m_bool_1.deserialize(context, __has_m_bool_1);
                        __has_m_bool_1 = true;
                        break;
                    case 1:
                        value.m_bool_2 = this.m_bool_2.deserialize(context, __has_m_bool_2);
                        __has_m_bool_2 = true;
                        break;
                    case 2:
                        value.m_bool_3 = this.m_bool_3.deserialize(context, __has_m_bool_3);
                        __has_m_bool_3 = true;
                        break;
                    case 3:
                        value.m_str_1 = this.m_str_1.deserialize(context, __has_m_str_1);
                        __has_m_str_1 = true;
                        break;
                    case 4:
                        value.m_str_2 = this.m_str_2.deserialize(context, __has_m_str_2);
                        __has_m_str_2 = true;
                        break;
                    case 5:
                        value.m_int8_4 = this.m_int8_4.deserialize(context, __has_m_int8_4);
                        __has_m_int8_4 = true;
                        break;
                    case 6:
                        value.m_int8_5 = this.m_int8_5.deserialize(context, __has_m_int8_5);
                        __has_m_int8_5 = true;
                        break;
                    case 7:
                        value.m_int16_4 = this.m_int16_4.deserialize(context, __has_m_int16_4);
                        __has_m_int16_4 = true;
                        break;
                    case 8:
                        value.m_int16_5 = this.m_int16_5.deserialize(context, __has_m_int16_5);
                        __has_m_int16_5 = true;
                        break;
                    case 9:
                        value.m_int32_4 = this.m_int32_4.deserialize(context, __has_m_int32_4);
                        __has_m_int32_4 = true;
                        break;
                    case 10:
                        value.m_int32_max = this.m_int32_max.deserialize(context, __has_m_int32_max);
                        __has_m_int32_max = true;
                        break;
                    case 11:
                        value.m_int64_4 = this.m_int64_4.deserialize(context, __has_m_int64_4);
                        __has_m_int64_4 = true;
                        break;
                    case 12:
                        value.m_int64_max = this.m_int64_max.deserialize(context, __has_m_int64_max);
                        __has_m_int64_max = true;
                        break;
                    case 13:
                        value.m_uint8_2 = this.m_uint8_2.deserialize(context, __has_m_uint8_2);
                        __has_m_uint8_2 = true;
                        break;
                    case 14:
                        value.m_uint8_3 = this.m_uint8_3.deserialize(context, __has_m_uint8_3);
                        __has_m_uint8_3 = true;
                        break;
                    case 15:
                        value.m_uint16_2 = this.m_uint16_2.deserialize(context, __has_m_uint16_2);
                        __has_m_uint16_2 = true;
                        break;
                    case 16:
                        value.m_uint16_3 = this.m_uint16_3.deserialize(context, __has_m_uint16_3);
                        __has_m_uint16_3 = true;
                        break;
                    case 17:
                        value.m_uint32_3 = this.m_uint32_3.deserialize(context, __has_m_uint32_3);
                        __has_m_uint32_3 = true;
                        break;
                    case 18:
                        value.m_uint32_max = this.m_uint32_max.deserialize(context, __has_m_uint32_max);
                        __has_m_uint32_max = true;
                        break;
                    case 19:
                        value.m_uint64_3 = this.m_uint64_3.deserialize(context, __has_m_uint64_3);
                        __has_m_uint64_3 = true;
                        break;
                    case 20:
                        value.m_uint64_max = this.m_uint64_max.deserialize(context, __has_m_uint64_max);
                        __has_m_uint64_max = true;
                        break;
                    case 21:
                        value.m_double_3 = this.m_double_3.deserialize(context, __has_m_double_3);
                        __has_m_double_3 = true;
                        break;
                    case 22:
                        value.m_double_4 = this.m_double_4.deserialize(context, __has_m_double_4);
                        __has_m_double_4 = true;
                        break;
                    case 23:
                        value.m_double_5 = this.m_double_5.deserialize(context, __has_m_double_5);
                        __has_m_double_5 = true;
                        break;
                    case 24:
                        value.m_float_3 = this.m_float_3.deserialize(context, __has_m_float_3);
                        __has_m_float_3 = true;
                        break;
                    case 25:
                        value.m_float_4 = this.m_float_4.deserialize(context, __has_m_float_4);
                        __has_m_float_4 = true;
                        break;
                    case 26:
                        value.m_float_7 = this.m_float_7.deserialize(context, __has_m_float_7);
                        __has_m_float_7 = true;
                        break;
                    case 27:
                        value.m_enum1 = this.m_enum1.deserialize(context, __has_m_enum1);
                        __has_m_enum1 = true;
                        break;
                    case 28:
                        value.m_enum2 = this.m_enum2.deserialize(context, __has_m_enum2);
                        __has_m_enum2 = true;
                        break;
                    case 29:
                        value.m_enum3 = this.m_enum3.deserialize(context, __has_m_enum3);
                        __has_m_enum3 = true;
                        break;
                    case 30:
                        value.m_enum_int32min = this.m_enum_int32min.deserialize(context, __has_m_enum_int32min);
                        __has_m_enum_int32min = true;
                        break;
                    case 31:
                        value.m_enum_int32max = this.m_enum_int32max.deserialize(context, __has_m_enum_int32max);
                        __has_m_enum_int32max = true;
                        break;
                    case 32:
                        value.m_enum_uint32_min = this.m_enum_uint32_min.deserialize(context, __has_m_enum_uint32_min);
                        __has_m_enum_uint32_min = true;
                        break;
                    case 33:
                        value.m_enum_uint32_max = this.m_enum_uint32_max.deserialize(context, __has_m_enum_uint32_max);
                        __has_m_enum_uint32_max = true;
                        break;
                    case 34:
                        value.m_wstr_1 = this.m_wstr_1.deserialize(context, __has_m_wstr_1);
                        __has_m_wstr_1 = true;
                        break;
                    case 35:
                        value.m_wstr_2 = this.m_wstr_2.deserialize(context, __has_m_wstr_2);
                        __has_m_wstr_2 = true;
                        break;
                    case 36:
                        value.m_int64_neg_hex = this.m_int64_neg_hex.deserialize(context, __has_m_int64_neg_hex);
                        __has_m_int64_neg_hex = true;
                        break;
                    case 37:
                        value.m_int64_neg_oct = this.m_int64_neg_oct.deserialize(context, __has_m_int64_neg_oct);
                        __has_m_int64_neg_oct = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.m_bool_1.verifyDeserialized(__has_m_bool_1);
            this.m_bool_2.verifyDeserialized(__has_m_bool_2);
            this.m_bool_3.verifyDeserialized(__has_m_bool_3);
            this.m_str_1.verifyDeserialized(__has_m_str_1);
            this.m_str_2.verifyDeserialized(__has_m_str_2);
            this.m_int8_4.verifyDeserialized(__has_m_int8_4);
            this.m_int8_5.verifyDeserialized(__has_m_int8_5);
            this.m_int16_4.verifyDeserialized(__has_m_int16_4);
            this.m_int16_5.verifyDeserialized(__has_m_int16_5);
            this.m_int32_4.verifyDeserialized(__has_m_int32_4);
            this.m_int32_max.verifyDeserialized(__has_m_int32_max);
            this.m_int64_4.verifyDeserialized(__has_m_int64_4);
            this.m_int64_max.verifyDeserialized(__has_m_int64_max);
            this.m_uint8_2.verifyDeserialized(__has_m_uint8_2);
            this.m_uint8_3.verifyDeserialized(__has_m_uint8_3);
            this.m_uint16_2.verifyDeserialized(__has_m_uint16_2);
            this.m_uint16_3.verifyDeserialized(__has_m_uint16_3);
            this.m_uint32_3.verifyDeserialized(__has_m_uint32_3);
            this.m_uint32_max.verifyDeserialized(__has_m_uint32_max);
            this.m_uint64_3.verifyDeserialized(__has_m_uint64_3);
            this.m_uint64_max.verifyDeserialized(__has_m_uint64_max);
            this.m_double_3.verifyDeserialized(__has_m_double_3);
            this.m_double_4.verifyDeserialized(__has_m_double_4);
            this.m_double_5.verifyDeserialized(__has_m_double_5);
            this.m_float_3.verifyDeserialized(__has_m_float_3);
            this.m_float_4.verifyDeserialized(__has_m_float_4);
            this.m_float_7.verifyDeserialized(__has_m_float_7);
            this.m_enum1.verifyDeserialized(__has_m_enum1);
            this.m_enum2.verifyDeserialized(__has_m_enum2);
            this.m_enum3.verifyDeserialized(__has_m_enum3);
            this.m_enum_int32min.verifyDeserialized(__has_m_enum_int32min);
            this.m_enum_int32max.verifyDeserialized(__has_m_enum_int32max);
            this.m_enum_uint32_min.verifyDeserialized(__has_m_enum_uint32_min);
            this.m_enum_uint32_max.verifyDeserialized(__has_m_enum_uint32_max);
            this.m_wstr_1.verifyDeserialized(__has_m_wstr_1);
            this.m_wstr_2.verifyDeserialized(__has_m_wstr_2);
            this.m_int64_neg_hex.verifyDeserialized(__has_m_int64_neg_hex);
            this.m_int64_neg_oct.verifyDeserialized(__has_m_int64_neg_oct);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo value) throws java.io.IOException {
            boolean __has_m_bool_1 = false;
            boolean __has_m_bool_2 = false;
            boolean __has_m_bool_3 = false;
            boolean __has_m_str_1 = false;
            boolean __has_m_str_2 = false;
            boolean __has_m_int8_4 = false;
            boolean __has_m_int8_5 = false;
            boolean __has_m_int16_4 = false;
            boolean __has_m_int16_5 = false;
            boolean __has_m_int32_4 = false;
            boolean __has_m_int32_max = false;
            boolean __has_m_int64_4 = false;
            boolean __has_m_int64_max = false;
            boolean __has_m_uint8_2 = false;
            boolean __has_m_uint8_3 = false;
            boolean __has_m_uint16_2 = false;
            boolean __has_m_uint16_3 = false;
            boolean __has_m_uint32_3 = false;
            boolean __has_m_uint32_max = false;
            boolean __has_m_uint64_3 = false;
            boolean __has_m_uint64_max = false;
            boolean __has_m_double_3 = false;
            boolean __has_m_double_4 = false;
            boolean __has_m_double_5 = false;
            boolean __has_m_float_3 = false;
            boolean __has_m_float_4 = false;
            boolean __has_m_float_7 = false;
            boolean __has_m_enum1 = false;
            boolean __has_m_enum2 = false;
            boolean __has_m_enum3 = false;
            boolean __has_m_enum_int32min = false;
            boolean __has_m_enum_int32max = false;
            boolean __has_m_enum_uint32_min = false;
            boolean __has_m_enum_uint32_max = false;
            boolean __has_m_wstr_1 = false;
            boolean __has_m_wstr_2 = false;
            boolean __has_m_int64_neg_hex = false;
            boolean __has_m_int64_neg_oct = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.m_bool_1 = this.m_bool_1.deserialize(context, field.type);
                        __has_m_bool_1 = true;
                        break;
                    case 1:
                        value.m_bool_2 = this.m_bool_2.deserialize(context, field.type);
                        __has_m_bool_2 = true;
                        break;
                    case 2:
                        value.m_bool_3 = this.m_bool_3.deserialize(context, field.type);
                        __has_m_bool_3 = true;
                        break;
                    case 3:
                        value.m_str_1 = this.m_str_1.deserialize(context, field.type);
                        __has_m_str_1 = true;
                        break;
                    case 4:
                        value.m_str_2 = this.m_str_2.deserialize(context, field.type);
                        __has_m_str_2 = true;
                        break;
                    case 5:
                        value.m_int8_4 = this.m_int8_4.deserialize(context, field.type);
                        __has_m_int8_4 = true;
                        break;
                    case 6:
                        value.m_int8_5 = this.m_int8_5.deserialize(context, field.type);
                        __has_m_int8_5 = true;
                        break;
                    case 7:
                        value.m_int16_4 = this.m_int16_4.deserialize(context, field.type);
                        __has_m_int16_4 = true;
                        break;
                    case 8:
                        value.m_int16_5 = this.m_int16_5.deserialize(context, field.type);
                        __has_m_int16_5 = true;
                        break;
                    case 9:
                        value.m_int32_4 = this.m_int32_4.deserialize(context, field.type);
                        __has_m_int32_4 = true;
                        break;
                    case 10:
                        value.m_int32_max = this.m_int32_max.deserialize(context, field.type);
                        __has_m_int32_max = true;
                        break;
                    case 11:
                        value.m_int64_4 = this.m_int64_4.deserialize(context, field.type);
                        __has_m_int64_4 = true;
                        break;
                    case 12:
                        value.m_int64_max = this.m_int64_max.deserialize(context, field.type);
                        __has_m_int64_max = true;
                        break;
                    case 13:
                        value.m_uint8_2 = this.m_uint8_2.deserialize(context, field.type);
                        __has_m_uint8_2 = true;
                        break;
                    case 14:
                        value.m_uint8_3 = this.m_uint8_3.deserialize(context, field.type);
                        __has_m_uint8_3 = true;
                        break;
                    case 15:
                        value.m_uint16_2 = this.m_uint16_2.deserialize(context, field.type);
                        __has_m_uint16_2 = true;
                        break;
                    case 16:
                        value.m_uint16_3 = this.m_uint16_3.deserialize(context, field.type);
                        __has_m_uint16_3 = true;
                        break;
                    case 17:
                        value.m_uint32_3 = this.m_uint32_3.deserialize(context, field.type);
                        __has_m_uint32_3 = true;
                        break;
                    case 18:
                        value.m_uint32_max = this.m_uint32_max.deserialize(context, field.type);
                        __has_m_uint32_max = true;
                        break;
                    case 19:
                        value.m_uint64_3 = this.m_uint64_3.deserialize(context, field.type);
                        __has_m_uint64_3 = true;
                        break;
                    case 20:
                        value.m_uint64_max = this.m_uint64_max.deserialize(context, field.type);
                        __has_m_uint64_max = true;
                        break;
                    case 21:
                        value.m_double_3 = this.m_double_3.deserialize(context, field.type);
                        __has_m_double_3 = true;
                        break;
                    case 22:
                        value.m_double_4 = this.m_double_4.deserialize(context, field.type);
                        __has_m_double_4 = true;
                        break;
                    case 23:
                        value.m_double_5 = this.m_double_5.deserialize(context, field.type);
                        __has_m_double_5 = true;
                        break;
                    case 24:
                        value.m_float_3 = this.m_float_3.deserialize(context, field.type);
                        __has_m_float_3 = true;
                        break;
                    case 25:
                        value.m_float_4 = this.m_float_4.deserialize(context, field.type);
                        __has_m_float_4 = true;
                        break;
                    case 26:
                        value.m_float_7 = this.m_float_7.deserialize(context, field.type);
                        __has_m_float_7 = true;
                        break;
                    case 27:
                        value.m_enum1 = this.m_enum1.deserialize(context, field.type);
                        __has_m_enum1 = true;
                        break;
                    case 28:
                        value.m_enum2 = this.m_enum2.deserialize(context, field.type);
                        __has_m_enum2 = true;
                        break;
                    case 29:
                        value.m_enum3 = this.m_enum3.deserialize(context, field.type);
                        __has_m_enum3 = true;
                        break;
                    case 30:
                        value.m_enum_int32min = this.m_enum_int32min.deserialize(context, field.type);
                        __has_m_enum_int32min = true;
                        break;
                    case 31:
                        value.m_enum_int32max = this.m_enum_int32max.deserialize(context, field.type);
                        __has_m_enum_int32max = true;
                        break;
                    case 32:
                        value.m_enum_uint32_min = this.m_enum_uint32_min.deserialize(context, field.type);
                        __has_m_enum_uint32_min = true;
                        break;
                    case 33:
                        value.m_enum_uint32_max = this.m_enum_uint32_max.deserialize(context, field.type);
                        __has_m_enum_uint32_max = true;
                        break;
                    case 34:
                        value.m_wstr_1 = this.m_wstr_1.deserialize(context, field.type);
                        __has_m_wstr_1 = true;
                        break;
                    case 35:
                        value.m_wstr_2 = this.m_wstr_2.deserialize(context, field.type);
                        __has_m_wstr_2 = true;
                        break;
                    case 36:
                        value.m_int64_neg_hex = this.m_int64_neg_hex.deserialize(context, field.type);
                        __has_m_int64_neg_hex = true;
                        break;
                    case 37:
                        value.m_int64_neg_oct = this.m_int64_neg_oct.deserialize(context, field.type);
                        __has_m_int64_neg_oct = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.m_bool_1.verifyDeserialized(__has_m_bool_1);
            this.m_bool_2.verifyDeserialized(__has_m_bool_2);
            this.m_bool_3.verifyDeserialized(__has_m_bool_3);
            this.m_str_1.verifyDeserialized(__has_m_str_1);
            this.m_str_2.verifyDeserialized(__has_m_str_2);
            this.m_int8_4.verifyDeserialized(__has_m_int8_4);
            this.m_int8_5.verifyDeserialized(__has_m_int8_5);
            this.m_int16_4.verifyDeserialized(__has_m_int16_4);
            this.m_int16_5.verifyDeserialized(__has_m_int16_5);
            this.m_int32_4.verifyDeserialized(__has_m_int32_4);
            this.m_int32_max.verifyDeserialized(__has_m_int32_max);
            this.m_int64_4.verifyDeserialized(__has_m_int64_4);
            this.m_int64_max.verifyDeserialized(__has_m_int64_max);
            this.m_uint8_2.verifyDeserialized(__has_m_uint8_2);
            this.m_uint8_3.verifyDeserialized(__has_m_uint8_3);
            this.m_uint16_2.verifyDeserialized(__has_m_uint16_2);
            this.m_uint16_3.verifyDeserialized(__has_m_uint16_3);
            this.m_uint32_3.verifyDeserialized(__has_m_uint32_3);
            this.m_uint32_max.verifyDeserialized(__has_m_uint32_max);
            this.m_uint64_3.verifyDeserialized(__has_m_uint64_3);
            this.m_uint64_max.verifyDeserialized(__has_m_uint64_max);
            this.m_double_3.verifyDeserialized(__has_m_double_3);
            this.m_double_4.verifyDeserialized(__has_m_double_4);
            this.m_double_5.verifyDeserialized(__has_m_double_5);
            this.m_float_3.verifyDeserialized(__has_m_float_3);
            this.m_float_4.verifyDeserialized(__has_m_float_4);
            this.m_float_7.verifyDeserialized(__has_m_float_7);
            this.m_enum1.verifyDeserialized(__has_m_enum1);
            this.m_enum2.verifyDeserialized(__has_m_enum2);
            this.m_enum3.verifyDeserialized(__has_m_enum3);
            this.m_enum_int32min.verifyDeserialized(__has_m_enum_int32min);
            this.m_enum_int32max.verifyDeserialized(__has_m_enum_int32max);
            this.m_enum_uint32_min.verifyDeserialized(__has_m_enum_uint32_min);
            this.m_enum_uint32_max.verifyDeserialized(__has_m_enum_uint32_max);
            this.m_wstr_1.verifyDeserialized(__has_m_wstr_1);
            this.m_wstr_2.verifyDeserialized(__has_m_wstr_2);
            this.m_int64_neg_hex.verifyDeserialized(__has_m_int64_neg_hex);
            this.m_int64_neg_oct.verifyDeserialized(__has_m_int64_neg_oct);
        }
        
        @Override
        protected final void initializeStructFields(Foo value) {
            value.m_bool_1 = this.m_bool_1.initialize();
            value.m_bool_2 = this.m_bool_2.initialize();
            value.m_bool_3 = this.m_bool_3.initialize();
            value.m_str_1 = this.m_str_1.initialize();
            value.m_str_2 = this.m_str_2.initialize();
            value.m_int8_4 = this.m_int8_4.initialize();
            value.m_int8_5 = this.m_int8_5.initialize();
            value.m_int16_4 = this.m_int16_4.initialize();
            value.m_int16_5 = this.m_int16_5.initialize();
            value.m_int32_4 = this.m_int32_4.initialize();
            value.m_int32_max = this.m_int32_max.initialize();
            value.m_int64_4 = this.m_int64_4.initialize();
            value.m_int64_max = this.m_int64_max.initialize();
            value.m_uint8_2 = this.m_uint8_2.initialize();
            value.m_uint8_3 = this.m_uint8_3.initialize();
            value.m_uint16_2 = this.m_uint16_2.initialize();
            value.m_uint16_3 = this.m_uint16_3.initialize();
            value.m_uint32_3 = this.m_uint32_3.initialize();
            value.m_uint32_max = this.m_uint32_max.initialize();
            value.m_uint64_3 = this.m_uint64_3.initialize();
            value.m_uint64_max = this.m_uint64_max.initialize();
            value.m_double_3 = this.m_double_3.initialize();
            value.m_double_4 = this.m_double_4.initialize();
            value.m_double_5 = this.m_double_5.initialize();
            value.m_float_3 = this.m_float_3.initialize();
            value.m_float_4 = this.m_float_4.initialize();
            value.m_float_7 = this.m_float_7.initialize();
            value.m_enum1 = this.m_enum1.initialize();
            value.m_enum2 = this.m_enum2.initialize();
            value.m_enum3 = this.m_enum3.initialize();
            value.m_enum_int32min = this.m_enum_int32min.initialize();
            value.m_enum_int32max = this.m_enum_int32max.initialize();
            value.m_enum_uint32_min = this.m_enum_uint32_min.initialize();
            value.m_enum_uint32_max = this.m_enum_uint32_max.initialize();
            value.m_wstr_1 = this.m_wstr_1.initialize();
            value.m_wstr_2 = this.m_wstr_2.initialize();
            value.m_int64_neg_hex = this.m_int64_neg_hex.initialize();
            value.m_int64_neg_oct = this.m_int64_neg_oct.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Foo fromValue, Foo toValue) {
            toValue.m_bool_1 = this.m_bool_1.clone(fromValue.m_bool_1);
            toValue.m_bool_2 = this.m_bool_2.clone(fromValue.m_bool_2);
            toValue.m_bool_3 = this.m_bool_3.clone(fromValue.m_bool_3);
            toValue.m_str_1 = this.m_str_1.clone(fromValue.m_str_1);
            toValue.m_str_2 = this.m_str_2.clone(fromValue.m_str_2);
            toValue.m_int8_4 = this.m_int8_4.clone(fromValue.m_int8_4);
            toValue.m_int8_5 = this.m_int8_5.clone(fromValue.m_int8_5);
            toValue.m_int16_4 = this.m_int16_4.clone(fromValue.m_int16_4);
            toValue.m_int16_5 = this.m_int16_5.clone(fromValue.m_int16_5);
            toValue.m_int32_4 = this.m_int32_4.clone(fromValue.m_int32_4);
            toValue.m_int32_max = this.m_int32_max.clone(fromValue.m_int32_max);
            toValue.m_int64_4 = this.m_int64_4.clone(fromValue.m_int64_4);
            toValue.m_int64_max = this.m_int64_max.clone(fromValue.m_int64_max);
            toValue.m_uint8_2 = this.m_uint8_2.clone(fromValue.m_uint8_2);
            toValue.m_uint8_3 = this.m_uint8_3.clone(fromValue.m_uint8_3);
            toValue.m_uint16_2 = this.m_uint16_2.clone(fromValue.m_uint16_2);
            toValue.m_uint16_3 = this.m_uint16_3.clone(fromValue.m_uint16_3);
            toValue.m_uint32_3 = this.m_uint32_3.clone(fromValue.m_uint32_3);
            toValue.m_uint32_max = this.m_uint32_max.clone(fromValue.m_uint32_max);
            toValue.m_uint64_3 = this.m_uint64_3.clone(fromValue.m_uint64_3);
            toValue.m_uint64_max = this.m_uint64_max.clone(fromValue.m_uint64_max);
            toValue.m_double_3 = this.m_double_3.clone(fromValue.m_double_3);
            toValue.m_double_4 = this.m_double_4.clone(fromValue.m_double_4);
            toValue.m_double_5 = this.m_double_5.clone(fromValue.m_double_5);
            toValue.m_float_3 = this.m_float_3.clone(fromValue.m_float_3);
            toValue.m_float_4 = this.m_float_4.clone(fromValue.m_float_4);
            toValue.m_float_7 = this.m_float_7.clone(fromValue.m_float_7);
            toValue.m_enum1 = this.m_enum1.clone(fromValue.m_enum1);
            toValue.m_enum2 = this.m_enum2.clone(fromValue.m_enum2);
            toValue.m_enum3 = this.m_enum3.clone(fromValue.m_enum3);
            toValue.m_enum_int32min = this.m_enum_int32min.clone(fromValue.m_enum_int32min);
            toValue.m_enum_int32max = this.m_enum_int32max.clone(fromValue.m_enum_int32max);
            toValue.m_enum_uint32_min = this.m_enum_uint32_min.clone(fromValue.m_enum_uint32_min);
            toValue.m_enum_uint32_max = this.m_enum_uint32_max.clone(fromValue.m_enum_uint32_max);
            toValue.m_wstr_1 = this.m_wstr_1.clone(fromValue.m_wstr_1);
            toValue.m_wstr_2 = this.m_wstr_2.clone(fromValue.m_wstr_2);
            toValue.m_int64_neg_hex = this.m_int64_neg_hex.clone(fromValue.m_int64_neg_hex);
            toValue.m_int64_neg_oct = this.m_int64_neg_oct.clone(fromValue.m_int64_neg_oct);
        }
    }

    public static final org.bondlib.StructBondType<Foo> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    
    // Java native serialization
    private static final long serialVersionUID = 0L;
    private Foo __deserializedInstance;

    @Override
    public void writeExternal(java.io.ObjectOutput out) throws java.io.IOException {
        final java.io.ByteArrayOutputStream outStream = new java.io.ByteArrayOutputStream();
        final org.bondlib.ProtocolWriter writer = new org.bondlib.CompactBinaryWriter(outStream, 1);
        org.bondlib.Marshal.marshal(this, writer);

        final byte[] marshalled = outStream.toByteArray();
        out.write(0);   // This type is not generic and has zero type parameters.
        out.writeInt(marshalled.length);
        out.write(marshalled);
    }

    @Override
    public void readExternal(java.io.ObjectInput in) throws java.io.IOException, java.lang.ClassNotFoundException {
        if (in.read() != 0) throw new java.io.IOException("type is not generic, but serialized data has type parameters.");
        final int marshalledLength = in.readInt();
        final byte[] marshalled = new byte[marshalledLength];
        in.readFully(marshalled);

        final java.io.ByteArrayInputStream inStream = new java.io.ByteArrayInputStream(marshalled);
        this.__deserializedInstance = org.bondlib.Unmarshal.unmarshal(inStream, getBondType()).deserialize();
    }

    private Object readResolve() throws java.io.ObjectStreamException {
        return this.__deserializedInstance;
    }
    // end Java native serialization
    

    public boolean m_bool_1;

    public boolean m_bool_2;

    public org.bondlib.SomethingBoolean m_bool_3;

    public java.lang.String m_str_1;

    public org.bondlib.SomethingObject<java.lang.String> m_str_2;

    public byte m_int8_4;

    public org.bondlib.SomethingByte m_int8_5;

    public short m_int16_4;

    public org.bondlib.SomethingShort m_int16_5;

    public org.bondlib.SomethingInteger m_int32_4;

    public int m_int32_max;

    public org.bondlib.SomethingLong m_int64_4;

    public long m_int64_max;

    public byte m_uint8_2;

    public org.bondlib.SomethingByte m_uint8_3;

    public short m_uint16_2;

    public org.bondlib.SomethingShort m_uint16_3;

    public org.bondlib.SomethingInteger m_uint32_3;

    public int m_uint32_max;

    public org.bondlib.SomethingLong m_uint64_3;

    public long m_uint64_max;

    public org.bondlib.SomethingDouble m_double_3;

    public double m_double_4;

    public double m_double_5;

    public org.bondlib.SomethingFloat m_float_3;

    public float m_float_4;

    public float m_float_7;

    public tests.EnumType1 m_enum1;

    public tests.EnumType1 m_enum2;

    public org.bondlib.SomethingObject<tests.EnumType1> m_enum3;

    public tests.EnumType1 m_enum_int32min;

    public tests.EnumType1 m_enum_int32max;

    public tests.EnumType1 m_enum_uint32_min;

    public tests.EnumType1 m_enum_uint32_max;

    public java.lang.String m_wstr_1;

    public org.bondlib.SomethingObject<java.lang.String> m_wstr_2;

    public long m_int64_neg_hex;

    public long m_int64_neg_oct;
    
    public Foo() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        
        final Foo other = (Foo) o;
        if (!(this.m_bool_1 == other.m_bool_1)) return false;
        if (!(this.m_bool_2 == other.m_bool_2)) return false;
        if (!((this.m_bool_3 == null && other.m_bool_3 == null) || (this.m_bool_3 != null && this.m_bool_3.equals(other.m_bool_3)))) return false;
        if (!((this.m_str_1 == null && other.m_str_1 == null) || (this.m_str_1 != null && this.m_str_1.equals(other.m_str_1)))) return false;
        if (!((this.m_str_2 == null && other.m_str_2 == null) || (this.m_str_2 != null && this.m_str_2.equals(other.m_str_2)))) return false;
        if (!(this.m_int8_4 == other.m_int8_4)) return false;
        if (!((this.m_int8_5 == null && other.m_int8_5 == null) || (this.m_int8_5 != null && this.m_int8_5.equals(other.m_int8_5)))) return false;
        if (!(this.m_int16_4 == other.m_int16_4)) return false;
        if (!((this.m_int16_5 == null && other.m_int16_5 == null) || (this.m_int16_5 != null && this.m_int16_5.equals(other.m_int16_5)))) return false;
        if (!((this.m_int32_4 == null && other.m_int32_4 == null) || (this.m_int32_4 != null && this.m_int32_4.equals(other.m_int32_4)))) return false;
        if (!(this.m_int32_max == other.m_int32_max)) return false;
        if (!((this.m_int64_4 == null && other.m_int64_4 == null) || (this.m_int64_4 != null && this.m_int64_4.equals(other.m_int64_4)))) return false;
        if (!(this.m_int64_max == other.m_int64_max)) return false;
        if (!(this.m_uint8_2 == other.m_uint8_2)) return false;
        if (!((this.m_uint8_3 == null && other.m_uint8_3 == null) || (this.m_uint8_3 != null && this.m_uint8_3.equals(other.m_uint8_3)))) return false;
        if (!(this.m_uint16_2 == other.m_uint16_2)) return false;
        if (!((this.m_uint16_3 == null && other.m_uint16_3 == null) || (this.m_uint16_3 != null && this.m_uint16_3.equals(other.m_uint16_3)))) return false;
        if (!((this.m_uint32_3 == null && other.m_uint32_3 == null) || (this.m_uint32_3 != null && this.m_uint32_3.equals(other.m_uint32_3)))) return false;
        if (!(this.m_uint32_max == other.m_uint32_max)) return false;
        if (!((this.m_uint64_3 == null && other.m_uint64_3 == null) || (this.m_uint64_3 != null && this.m_uint64_3.equals(other.m_uint64_3)))) return false;
        if (!(this.m_uint64_max == other.m_uint64_max)) return false;
        if (!((this.m_double_3 == null && other.m_double_3 == null) || (this.m_double_3 != null && this.m_double_3.equals(other.m_double_3)))) return false;
        if (!(org.bondlib.FloatingPointHelper.doubleEquals(this.m_double_4, other.m_double_4))) return false;
        if (!(org.bondlib.FloatingPointHelper.doubleEquals(this.m_double_5, other.m_double_5))) return false;
        if (!((this.m_float_3 == null && other.m_float_3 == null) || (this.m_float_3 != null && this.m_float_3.equals(other.m_float_3)))) return false;
        if (!(org.bondlib.FloatingPointHelper.floatEquals(this.m_float_4, other.m_float_4))) return false;
        if (!(org.bondlib.FloatingPointHelper.floatEquals(this.m_float_7, other.m_float_7))) return false;
        if (!((this.m_enum1 == null && other.m_enum1 == null) || (this.m_enum1 != null && this.m_enum1.equals(other.m_enum1)))) return false;
        if (!((this.m_enum2 == null && other.m_enum2 == null) || (this.m_enum2 != null && this.m_enum2.equals(other.m_enum2)))) return false;
        if (!((this.m_enum3 == null && other.m_enum3 == null) || (this.m_enum3 != null && this.m_enum3.equals(other.m_enum3)))) return false;
        if (!((this.m_enum_int32min == null && other.m_enum_int32min == null) || (this.m_enum_int32min != null && this.m_enum_int32min.equals(other.m_enum_int32min)))) return false;
        if (!((this.m_enum_int32max == null && other.m_enum_int32max == null) || (this.m_enum_int32max != null && this.m_enum_int32max.equals(other.m_enum_int32max)))) return false;
        if (!((this.m_enum_uint32_min == null && other.m_enum_uint32_min == null) || (this.m_enum_uint32_min != null && this.m_enum_uint32_min.equals(other.m_enum_uint32_min)))) return false;
        if (!((this.m_enum_uint32_max == null && other.m_enum_uint32_max == null) || (this.m_enum_uint32_max != null && this.m_enum_uint32_max.equals(other.m_enum_uint32_max)))) return false;
        if (!((this.m_wstr_1 == null && other.m_wstr_1 == null) || (this.m_wstr_1 != null && this.m_wstr_1.equals(other.m_wstr_1)))) return false;
        if (!((this.m_wstr_2 == null && other.m_wstr_2 == null) || (this.m_wstr_2 != null && this.m_wstr_2.equals(other.m_wstr_2)))) return false;
        if (!(this.m_int64_neg_hex == other.m_int64_neg_hex)) return false;
        if (!(this.m_int64_neg_oct == other.m_int64_neg_oct)) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += (m_bool_1 ? 0 : 1);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += (m_bool_2 ? 0 : 1);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_bool_3 == null ? 0 : m_bool_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_str_1 == null ? 0 : m_str_1.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_str_2 == null ? 0 : m_str_2.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int8_4;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int8_5 == null ? 0 : m_int8_5.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int16_4;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int16_5 == null ? 0 : m_int16_5.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int32_4 == null ? 0 : m_int32_4.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int32_max;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int64_4 == null ? 0 : m_int64_4.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int64_max ^ (m_int64_max >>> 32);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint8_2;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint8_3 == null ? 0 : m_uint8_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint16_2;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint16_3 == null ? 0 : m_uint16_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint32_3 == null ? 0 : m_uint32_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint32_max;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint64_3 == null ? 0 : m_uint64_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_uint64_max ^ (m_uint64_max >>> 32);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_double_3 == null ? 0 : m_double_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += org.bondlib.FloatingPointHelper.doubleHashCode(m_double_4);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += org.bondlib.FloatingPointHelper.doubleHashCode(m_double_5);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_float_3 == null ? 0 : m_float_3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += org.bondlib.FloatingPointHelper.floatHashCode(m_float_4);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += org.bondlib.FloatingPointHelper.floatHashCode(m_float_7);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum1 == null ? 0 : m_enum1.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum2 == null ? 0 : m_enum2.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum3 == null ? 0 : m_enum3.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum_int32min == null ? 0 : m_enum_int32min.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum_int32max == null ? 0 : m_enum_int32max.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum_uint32_min == null ? 0 : m_enum_uint32_min.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_enum_uint32_max == null ? 0 : m_enum_uint32_max.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_wstr_1 == null ? 0 : m_wstr_1.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_wstr_2 == null ? 0 : m_wstr_2.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int64_neg_hex ^ (m_int64_neg_hex >>> 32);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m_int64_neg_oct ^ (m_int64_neg_oct >>> 32);
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}
