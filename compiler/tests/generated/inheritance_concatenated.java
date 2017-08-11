
package tests;

@javax.annotation.Generated("gbc")
public class Base implements com.microsoft.bond.BondSerializable {
    
    private static final class StructBondTypeImpl extends com.microsoft.bond.StructBondType<Base> {
        
        static final class StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<Base> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final com.microsoft.bond.StructBondType<Base> buildNewInstance(com.microsoft.bond.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(Base.class, new StructBondTypeBuilderImpl());
            }
        }

        private com.microsoft.bond.StructBondType.Int32StructField x;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.x = new com.microsoft.bond.StructBondType.Int32StructField(this, 0, "x", com.microsoft.bond.Modifier.Optional);
            super.initializeBaseAndFields(null, this.x);
        }

        @Override
        public final java.lang.Class<Base> getValueClass() {
            return (java.lang.Class<Base>) (java.lang.Class) Base.class;
        }

        @Override
        public final Base newInstance() {
            return new Base();
        }
        
        @Override
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, Base value) throws java.io.IOException {
            this.x.serialize(context, value.x);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, Base value) throws java.io.IOException {
            boolean __has_x = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.x = this.x.deserialize(context, __has_x);
                        __has_x = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.x.verifyDeserialized(__has_x);
        }
        
        @Override
        protected final void initializeStructFields(Base value) {
            value.x = this.x.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Base fromValue, Base toValue) {
            toValue.x = this.x.clone(fromValue.x);
        }
    }

    public static final com.microsoft.bond.StructBondType<Base> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    public int x;
    
    public Base() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };


    @Override
    public com.microsoft.bond.StructBondType<? extends Base> getBondType() {
        return BOND_TYPE;
    }
}

package tests;

@javax.annotation.Generated("gbc")
public class Foo extends tests.Base {
    
    private static final class StructBondTypeImpl extends com.microsoft.bond.StructBondType<Foo> {
        
        static final class StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<Foo> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final com.microsoft.bond.StructBondType<Foo> buildNewInstance(com.microsoft.bond.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(Foo.class, new StructBondTypeBuilderImpl());
            }
        }

        private com.microsoft.bond.StructBondType.Int32StructField x;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.x = new com.microsoft.bond.StructBondType.Int32StructField(this, 0, "x", com.microsoft.bond.Modifier.Optional);
            super.initializeBaseAndFields((com.microsoft.bond.StructBondType<tests.Base>) getStructType(tests.Base.class), this.x);
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
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, Foo value) throws java.io.IOException {
            this.x.serialize(context, value.x);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
            boolean __has_x = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.x = this.x.deserialize(context, __has_x);
                        __has_x = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.x.verifyDeserialized(__has_x);
        }
        
        @Override
        protected final void initializeStructFields(Foo value) {
            value.x = this.x.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Foo fromValue, Foo toValue) {
            toValue.x = this.x.clone(fromValue.x);
        }
    }

    public static final com.microsoft.bond.StructBondType<Foo> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    public int x;
    
    public Foo() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };


    @Override
    public com.microsoft.bond.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}
