
package tests;

@javax.annotation.Generated("gbc")
public class Foo<T1, T2> implements com.microsoft.bond.BondSerializable {
    
    public static abstract class GenericBondTypeBuilder {

        private GenericBondTypeBuilder() {
        }

        public abstract <T1, T2> com.microsoft.bond.StructBondType<Foo<T1, T2>> makeGenericType(com.microsoft.bond.BondType<T1> T1, com.microsoft.bond.BondType<T2> T2);
    }

    private static final class StructBondTypeImpl<T1, T2> extends com.microsoft.bond.StructBondType<Foo<T1, T2>> {
        
        static final class StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<Foo> {
            
            private <T1, T2> com.microsoft.bond.StructBondType<Foo<T1, T2>> makeGenericType(com.microsoft.bond.BondType<T1> T1, com.microsoft.bond.BondType<T2> T2) {
                com.microsoft.bond.helpers.ArgumentHelper.ensureNotNull(T1, "T1");
                com.microsoft.bond.helpers.ArgumentHelper.ensureNotNull(T2, "T2");
                return (StructBondTypeImpl) this.getInitializedFromCache(T1, T2);
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 2;
            }

            @Override
            protected final com.microsoft.bond.StructBondType<Foo> buildNewInstance(com.microsoft.bond.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(new com.microsoft.bond.GenericTypeSpecialization(genericTypeArguments));
            }

            static void register() {
                registerStructType(Foo.class, new StructBondTypeBuilderImpl());
            }
        }

        private com.microsoft.bond.StructBondType.ObjectStructField<T2> t2;

        private com.microsoft.bond.StructBondType.ObjectStructField<tests.Foo<T1, java.lang.Boolean>> n;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            com.microsoft.bond.BondType<T1> T1 = this.getGenericSpecialization().getGenericTypeArgument(0);
            com.microsoft.bond.BondType<T2> T2 = this.getGenericSpecialization().getGenericTypeArgument(1);
            this.t2 = new com.microsoft.bond.StructBondType.ObjectStructField<T2>(this, T2, 0, "t2", com.microsoft.bond.Modifier.Optional);
            this.n = new com.microsoft.bond.StructBondType.ObjectStructField<tests.Foo<T1, java.lang.Boolean>>(this, nullableOf((com.microsoft.bond.StructBondType<tests.Foo<T1, java.lang.Boolean>>) getStructType(tests.Foo.class, T1, com.microsoft.bond.BondTypes.BOOL)), 1, "n", com.microsoft.bond.Modifier.Optional);
            super.initializeBaseAndFields(null, this.t2, this.n);
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
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, Foo<T1, T2> value) throws java.io.IOException {
            this.t2.serialize(context, value.t2);
            this.n.serialize(context, value.n);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, Foo<T1, T2> value) throws java.io.IOException {
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
        protected final void deserializeStructFields(com.microsoft.bond.BondType.UntaggedDeserializationContext context, Foo<T1, T2> value) throws java.io.IOException {
            value.t2 = this.t2.deserialize(context);
            value.n = this.n.deserialize(context);
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
        public final <T1, T2> com.microsoft.bond.StructBondType<Foo<T1, T2>> makeGenericType(com.microsoft.bond.BondType<T1> T1, com.microsoft.bond.BondType<T2> T2) {
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

    public T2 t2;

    public tests.Foo<T1, java.lang.Boolean> n;
    
public Foo(com.microsoft.bond.StructBondType<Foo<T1, T2>> genericType) {
        super();
        this.__genericType = (StructBondTypeImpl<T1, T2>)genericType;
        this.__genericType.initializeStructFields(this);
    };


    @Override
    public com.microsoft.bond.StructBondType<? extends Foo<T1, T2>> getBondType() {
        return this.__genericType;
    }
}
