
package tests;

@javax.annotation.Generated("gbc")
public class Foo<T1, T2> implements org.bondlib.BondSerializable {
    
    public static abstract class GenericBondTypeBuilder {

        private GenericBondTypeBuilder() {
        }

        public abstract <T1, T2> org.bondlib.StructBondType<Foo<T1, T2>> makeGenericType(org.bondlib.BondType<T1> T1, org.bondlib.BondType<T2> T2);
    }

    private static final class StructBondTypeImpl<T1, T2> extends org.bondlib.StructBondType<Foo<T1, T2>> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<Foo> {
            
            private <T1, T2> org.bondlib.StructBondType<Foo<T1, T2>> makeGenericType(org.bondlib.BondType<T1> T1, org.bondlib.BondType<T2> T2) {
                org.bondlib.ArgumentHelper.ensureNotNull(T1, "T1");
                org.bondlib.ArgumentHelper.ensureNotNull(T2, "T2");
                return (StructBondTypeImpl) this.getInitializedFromCache(T1, T2);
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 2;
            }

            @Override
            protected final org.bondlib.StructBondType<Foo> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(new org.bondlib.GenericTypeSpecialization(genericTypeArguments));
            }

            static void register() {
                registerStructType(Foo.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.ObjectStructField<T2> t2;

        private org.bondlib.StructBondType.ObjectStructField<tests.Foo<T1, java.lang.Boolean>> n;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            org.bondlib.BondType<T1> T1 = this.getGenericSpecialization().getGenericTypeArgument(0);
            org.bondlib.BondType<T2> T2 = this.getGenericSpecialization().getGenericTypeArgument(1);
            this.t2 = new org.bondlib.StructBondType.ObjectStructField<T2>(this, T2, 0, "t2", org.bondlib.Modifier.Optional);
            this.n = new org.bondlib.StructBondType.ObjectStructField<tests.Foo<T1, java.lang.Boolean>>(this, nullableOf((org.bondlib.StructBondType<tests.Foo<T1, java.lang.Boolean>>) getStructType(tests.Foo.class, T1, org.bondlib.BondTypes.BOOL)), 1, "n", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.t2, this.n);
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
        public final java.lang.Class<Foo<T1, T2>> getValueClass() {
            return (java.lang.Class<Foo<T1, T2>>) (java.lang.Class) Foo.class;
        }

        @Override
        public final Foo<T1, T2> newInstance() {
            return new Foo<T1, T2>(this);
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, Foo<T1, T2> value) throws java.io.IOException {
            this.t2.serialize(context, value.t2);
            this.n.serialize(context, value.n);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo<T1, T2> value) throws java.io.IOException {
            boolean __has_t2 = false;
            boolean __has_n = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.t2 = this.t2.deserialize(context, __has_t2);
                        __has_t2 = true;
                        break;
                    case 1:
                        value.n = this.n.deserialize(context, __has_n);
                        __has_n = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.t2.verifyDeserialized(__has_t2);
            this.n.verifyDeserialized(__has_n);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo<T1, T2> value) throws java.io.IOException {
            boolean __has_t2 = false;
            boolean __has_n = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.t2 = this.t2.deserialize(context, field.type);
                        __has_t2 = true;
                        break;
                    case 1:
                        value.n = this.n.deserialize(context, field.type);
                        __has_n = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.t2.verifyDeserialized(__has_t2);
            this.n.verifyDeserialized(__has_n);
        }
        
        @Override
        protected final void initializeStructFields(Foo<T1, T2> value) {
            value.t2 = this.t2.initialize();
            value.n = this.n.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Foo<T1, T2> fromValue, Foo<T1, T2> toValue) {
            toValue.t2 = this.t2.clone(fromValue.t2);
            toValue.n = this.n.clone(fromValue.n);
        }
    }

    public static final GenericBondTypeBuilder BOND_TYPE = new GenericBondTypeBuilder() {

        final StructBondTypeImpl.StructBondTypeBuilderImpl builder = new StructBondTypeImpl.StructBondTypeBuilderImpl();

        @Override
        public final <T1, T2> org.bondlib.StructBondType<Foo<T1, T2>> makeGenericType(org.bondlib.BondType<T1> T1, org.bondlib.BondType<T2> T2) {
            return this.builder.makeGenericType(T1, T2);
        }
    };

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    private final StructBondTypeImpl<T1, T2> __genericType;

    
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
    

    public T2 t2;

    public tests.Foo<T1, java.lang.Boolean> n;
    
public Foo(org.bondlib.StructBondType<Foo<T1, T2>> genericType) {
        super();
        this.__genericType = (StructBondTypeImpl<T1, T2>)genericType;
        this.__genericType.initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        
        final Foo other = (Foo) o;
        if (!((this.t2 == null && other.t2 == null) || (this.t2 != null && this.t2.equals(other.t2)))) return false;
        if (!((this.n == null && other.n == null) || (this.n != null && this.n.equals(other.n)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += t2 == null ? 0 : t2.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += n == null ? 0 : n.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo<T1, T2>> getBondType() {
        return this.__genericType;
    }
}
