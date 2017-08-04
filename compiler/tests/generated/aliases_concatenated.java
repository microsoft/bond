
package tests;

@javax.annotation.Generated("gbc")
public class Foo<T> implements com.microsoft.bond.BondSerializable {
    
    public static abstract class GenericBondTypeBuilder {

        private GenericBondTypeBuilder() {
        }

        public abstract <T> com.microsoft.bond.StructBondType<Foo<T>> makeGenericType(com.microsoft.bond.BondType<T> T);
    }

    private static final class StructBondTypeImpl<T> extends com.microsoft.bond.StructBondType<Foo<T>> {
        
        static final class StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<Foo> {
            
            private <T> com.microsoft.bond.StructBondType<Foo<T>> makeGenericType(com.microsoft.bond.BondType<T> T) {
                com.microsoft.bond.helpers.ArgumentHelper.ensureNotNull(T, "T");
                return (StructBondTypeImpl) this.getInitializedFromCache(T);
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 1;
            }

            @Override
            protected final com.microsoft.bond.StructBondType<Foo> buildNewInstance(com.microsoft.bond.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(new com.microsoft.bond.GenericTypeSpecialization(genericTypeArguments));
            }

            static void register() {
                registerStructType(Foo.class, new StructBondTypeBuilderImpl());
            }
        }

        private com.microsoft.bond.StructBondType.ObjectStructField<java.util.List<java.util.List<T>>> aa;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            com.microsoft.bond.BondType<T> T = this.getGenericSpecialization().getGenericTypeArgument(0);
            this.aa = new com.microsoft.bond.StructBondType.ObjectStructField<java.util.List<java.util.List<T>>>(this, vectorOf(vectorOf(T)), 0, "aa", com.microsoft.bond.Modifier.Optional);
            super.initializeBaseAndFields(null, this.aa);
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
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, Foo<T> value) throws java.io.IOException {
            this.aa.serialize(context, value.aa);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, Foo<T> value) throws java.io.IOException {
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
        protected final void deserializeStructFields(com.microsoft.bond.BondType.UntaggedDeserializationContext context, Foo<T> value) throws java.io.IOException {
            value.aa = this.aa.deserialize(context);
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
        public final <T> com.microsoft.bond.StructBondType<Foo<T>> makeGenericType(com.microsoft.bond.BondType<T> T) {
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

    public java.util.List<java.util.List<T>> aa;
    
public Foo(com.microsoft.bond.StructBondType<Foo<T>> genericType) {
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
    public com.microsoft.bond.StructBondType<? extends Foo<T>> getBondType() {
        return this.__genericType;
    }
}

package tests;


@javax.annotation.Generated("gbc")
public final class EnumToWrap implements com.microsoft.bond.BondEnum<EnumToWrap> {

    public static final class Values {
        private Values() {}

        public static final int anEnumValue = 0;
    }

    private static final class EnumBondTypeImpl extends com.microsoft.bond.EnumBondType<EnumToWrap> {

        @Override
        public java.lang.Class<EnumToWrap> getValueClass() { return EnumToWrap.class; }

        @Override
        public final EnumToWrap getEnumValue(int value) { return get(value); }
    }

    public static final com.microsoft.bond.EnumBondType<EnumToWrap> BOND_TYPE = new EnumBondTypeImpl();

    public static final EnumToWrap anEnumValue = new EnumToWrap(Values.anEnumValue, "anEnumValue");

    public final int value;

    private final java.lang.String label;

    private EnumToWrap(int value, String label) { this.value = value; this.label = label; }

    @Override
    public final int getValue() { return this.value; }

    @Override
    public final String getLabel() { return this.label; }

    @Override
    public final com.microsoft.bond.EnumBondType<EnumToWrap> getBondType() { return BOND_TYPE; }

    @Override
    public final int compareTo(EnumToWrap o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public final boolean equals(java.lang.Object other) { return (other instanceof EnumToWrap) && (this.value == ((EnumToWrap) other).value); }

    @Override
    public final int hashCode() { return this.value; }

    @Override
    public final java.lang.String toString() { return this.label != null ? this.label : ("EnumToWrap(" + String.valueOf(this.value) + ")"); }

    public static EnumToWrap get(int value) {
        switch (value) {
            case Values.anEnumValue: return anEnumValue;
            default: return new EnumToWrap(value, null);
        }
    }

    public static EnumToWrap valueOf(String str) {
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
public class WrappingAnEnum implements com.microsoft.bond.BondSerializable {
    
    private static final class StructBondTypeImpl extends com.microsoft.bond.StructBondType<WrappingAnEnum> {
        
        static final class StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<WrappingAnEnum> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final com.microsoft.bond.StructBondType<WrappingAnEnum> buildNewInstance(com.microsoft.bond.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(WrappingAnEnum.class, new StructBondTypeBuilderImpl());
            }
        }

        private com.microsoft.bond.StructBondType.EnumStructField<tests.EnumToWrap> aWrappedEnum;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.aWrappedEnum = new com.microsoft.bond.StructBondType.EnumStructField<tests.EnumToWrap>(this, tests.EnumToWrap.BOND_TYPE, 0, "aWrappedEnum", com.microsoft.bond.Modifier.Optional);
            super.initializeBaseAndFields(null, this.aWrappedEnum);
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
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, WrappingAnEnum value) throws java.io.IOException {
            this.aWrappedEnum.serialize(context, value.aWrappedEnum);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, WrappingAnEnum value) throws java.io.IOException {
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
        protected final void deserializeStructFields(com.microsoft.bond.BondType.UntaggedDeserializationContext context, WrappingAnEnum value) throws java.io.IOException {
            value.aWrappedEnum = this.aWrappedEnum.deserialize(context);
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

    public static final com.microsoft.bond.StructBondType<WrappingAnEnum> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

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
    public com.microsoft.bond.StructBondType<? extends WrappingAnEnum> getBondType() {
        return BOND_TYPE;
    }
}
