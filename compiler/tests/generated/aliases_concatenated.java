
package tests;

@javax.annotation.Generated("gbc")
public class Foo<T> implements org.bondlib.BondSerializable {
    
    public static abstract class GenericBondTypeBuilder {

        private GenericBondTypeBuilder() {
        }

        public abstract <T> org.bondlib.StructBondType<Foo<T>> makeGenericType(org.bondlib.BondType<T> T);
    }

    private static final class StructBondTypeImpl<T> extends org.bondlib.StructBondType<Foo<T>> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<Foo> {
            
            private <T> org.bondlib.StructBondType<Foo<T>> makeGenericType(org.bondlib.BondType<T> T) {
                org.bondlib.ArgumentHelper.ensureNotNull(T, "T");
                return (StructBondTypeImpl) this.getInitializedFromCache(T);
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 1;
            }

            @Override
            protected final org.bondlib.StructBondType<Foo> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(new org.bondlib.GenericTypeSpecialization(genericTypeArguments));
            }

            static void register() {
                registerStructType(Foo.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.ObjectStructField<java.util.List<java.util.List<T>>> aa;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            org.bondlib.BondType<T> T = this.getGenericSpecialization().getGenericTypeArgument(0);
            this.aa = new org.bondlib.StructBondType.ObjectStructField<java.util.List<java.util.List<T>>>(this, vectorOf(vectorOf(T)), 0, "aa", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.aa);
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
        public final java.lang.Class<Foo<T>> getValueClass() {
            return (java.lang.Class<Foo<T>>) (java.lang.Class) Foo.class;
        }

        @Override
        public final Foo<T> newInstance() {
            return new Foo<T>(this);
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, Foo<T> value) throws java.io.IOException {
            this.aa.serialize(context, value.aa);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo<T> value) throws java.io.IOException {
            boolean __has_aa = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.aa = this.aa.deserialize(context, __has_aa);
                        __has_aa = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.aa.verifyDeserialized(__has_aa);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo<T> value) throws java.io.IOException {
            boolean __has_aa = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.aa = this.aa.deserialize(context, field.type);
                        __has_aa = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.aa.verifyDeserialized(__has_aa);
        }
        
        @Override
        protected final void initializeStructFields(Foo<T> value) {
            value.aa = this.aa.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Foo<T> fromValue, Foo<T> toValue) {
            toValue.aa = this.aa.clone(fromValue.aa);
        }
    }

    public static final GenericBondTypeBuilder BOND_TYPE = new GenericBondTypeBuilder() {

        final StructBondTypeImpl.StructBondTypeBuilderImpl builder = new StructBondTypeImpl.StructBondTypeBuilderImpl();

        @Override
        public final <T> org.bondlib.StructBondType<Foo<T>> makeGenericType(org.bondlib.BondType<T> T) {
            return this.builder.makeGenericType(T);
        }
    };

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    private final StructBondTypeImpl<T> __genericType;

    
    // Java native serialization
    @Override
    public void writeExternal(java.io.ObjectOutput out) throws java.io.IOException {
        throw new java.lang.IllegalArgumentException("java.io.Serializable support is not implemented for generic types");
    }

    @Override
    public void readExternal(java.io.ObjectInput in) throws java.io.IOException, java.lang.ClassNotFoundException {
        // This may actually fail before reaching this line with an InvalidClassException because
        // generic types don't have the nullary constructor required by the Java serialization
        // framework.
        throw new java.lang.IllegalArgumentException("java.io.Serializable support is not implemented for generic types");
    }
    // end Java native serialization
    

    public java.util.List<java.util.List<T>> aa;
    
public Foo(org.bondlib.StructBondType<Foo<T>> genericType) {
        super();
        this.__genericType = (StructBondTypeImpl<T>)genericType;
        this.__genericType.initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        
        final Foo other = (Foo) o;
        if (!((this.aa == null && other.aa == null) || (this.aa != null && this.aa.equals(other.aa)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += aa == null ? 0 : aa.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo<T>> getBondType() {
        return this.__genericType;
    }
}

package tests;


@javax.annotation.Generated("gbc")
public final class EnumToWrap implements org.bondlib.BondEnum<EnumToWrap> {

    public static final class Values {
        private Values() {}

        public static final int anEnumValue = 0;
    }

    private static final class EnumBondTypeImpl extends org.bondlib.EnumBondType<EnumToWrap> {

        @Override
        public java.lang.Class<EnumToWrap> getValueClass() { return EnumToWrap.class; }

        @Override
        public final EnumToWrap getEnumValue(int value) { return get(value); }
    }

    public static final org.bondlib.EnumBondType<EnumToWrap> BOND_TYPE = new EnumBondTypeImpl();

    public static final EnumToWrap anEnumValue = new EnumToWrap(Values.anEnumValue, "anEnumValue");

    public final int value;

    private final java.lang.String label;

    private EnumToWrap(int value, java.lang.String label) { this.value = value; this.label = label; }

    @Override
    public final int getValue() { return this.value; }

    @Override
    public final java.lang.String getLabel() { return this.label; }

    @Override
    public final org.bondlib.EnumBondType<EnumToWrap> getBondType() { return BOND_TYPE; }

    @Override
    public final int compareTo(EnumToWrap o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public final boolean equals(java.lang.Object other) { return (other instanceof EnumToWrap) && (this.value == ((EnumToWrap) other).value); }

    @Override
    public final int hashCode() { return this.value; }

    @Override
    public final java.lang.String toString() { return this.label != null ? this.label : ("EnumToWrap(" + java.lang.String.valueOf(this.value) + ")"); }

    public static EnumToWrap get(int value) {
        switch (value) {
            case Values.anEnumValue: return anEnumValue;
            default: return new EnumToWrap(value, null);
        }
    }

    public static EnumToWrap valueOf(java.lang.String str) {
        if (str == null) {
            throw new java.lang.IllegalArgumentException("Argument 'str' must not be null.");
        } else if (str.equals("anEnumValue")) {
            return anEnumValue;
        } else {
            throw new java.lang.IllegalArgumentException("Invalid 'EnumToWrap' enum value: '" + str + "'.");
        }
    }
}

package tests;

@javax.annotation.Generated("gbc")
public class WrappingAnEnum implements org.bondlib.BondSerializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<WrappingAnEnum> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<WrappingAnEnum> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<WrappingAnEnum> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(WrappingAnEnum.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.EnumStructField<tests.EnumToWrap> aWrappedEnum;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.aWrappedEnum = new org.bondlib.StructBondType.EnumStructField<tests.EnumToWrap>(this, tests.EnumToWrap.BOND_TYPE, 0, "aWrappedEnum", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.aWrappedEnum);
        }

        @Override
        public final java.lang.String getName() {
            return "WrappingAnEnum";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "tests.WrappingAnEnum";
        }

        @Override
        public final java.lang.Class<WrappingAnEnum> getValueClass() {
            return (java.lang.Class<WrappingAnEnum>) (java.lang.Class) WrappingAnEnum.class;
        }

        @Override
        public final WrappingAnEnum newInstance() {
            return new WrappingAnEnum();
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, WrappingAnEnum value) throws java.io.IOException {
            this.aWrappedEnum.serialize(context, value.aWrappedEnum);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, WrappingAnEnum value) throws java.io.IOException {
            boolean __has_aWrappedEnum = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.aWrappedEnum = this.aWrappedEnum.deserialize(context, __has_aWrappedEnum);
                        __has_aWrappedEnum = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.aWrappedEnum.verifyDeserialized(__has_aWrappedEnum);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, WrappingAnEnum value) throws java.io.IOException {
            boolean __has_aWrappedEnum = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.aWrappedEnum = this.aWrappedEnum.deserialize(context, field.type);
                        __has_aWrappedEnum = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.aWrappedEnum.verifyDeserialized(__has_aWrappedEnum);
        }
        
        @Override
        protected final void initializeStructFields(WrappingAnEnum value) {
            value.aWrappedEnum = this.aWrappedEnum.initialize();
        }
        
        @Override
        protected final void cloneStructFields(WrappingAnEnum fromValue, WrappingAnEnum toValue) {
            toValue.aWrappedEnum = this.aWrappedEnum.clone(fromValue.aWrappedEnum);
        }
    }

    public static final org.bondlib.StructBondType<WrappingAnEnum> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    
    // Java native serialization
    private static final long serialVersionUID = 0L;
    private WrappingAnEnum __deserializedInstance;

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
    

    public tests.EnumToWrap aWrappedEnum;
    
    public WrappingAnEnum() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof WrappingAnEnum)) return false;
        
        final WrappingAnEnum other = (WrappingAnEnum) o;
        if (!((this.aWrappedEnum == null && other.aWrappedEnum == null) || (this.aWrappedEnum != null && this.aWrappedEnum.equals(other.aWrappedEnum)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += aWrappedEnum == null ? 0 : aWrappedEnum.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends WrappingAnEnum> getBondType() {
        return BOND_TYPE;
    }
}
