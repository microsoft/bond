
package tests;

@javax.annotation.Generated("gbc")
public class Base implements org.bondlib.BondSerializable, java.io.Serializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<Base> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<Base> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<Base> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(Base.class, new StructBondTypeBuilderImpl());
            }
        }

        private transient org.bondlib.StructBondType.Int32StructField x;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.x = new org.bondlib.StructBondType.Int32StructField(this, 0, "x", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.x);
        }

        @Override
        public final java.lang.String getName() {
            return "Base";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "tests.Base";
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
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, Base value) throws java.io.IOException {
            this.x.serialize(context, value.x);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Base value) throws java.io.IOException {
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
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Base value) throws java.io.IOException {
            boolean __has_x = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.x = this.x.deserialize(context, field.type);
                        __has_x = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
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

        private java.lang.Object readResolve() throws java.io.ObjectStreamException {
            return getCachedType(this, true);
        }
    }

    public static final org.bondlib.StructBondType<Base> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

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
    public boolean equals(Object o) {
        if (!(o instanceof Base)) return false;
        
        final Base other = (Base) o;
        if (!(this.x == other.x)) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += x;
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Base> getBondType() {
        return BOND_TYPE;
    }
}

package tests;

@javax.annotation.Generated("gbc")
public class Foo extends tests.Base {
    
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

        private transient org.bondlib.StructBondType.Int32StructField x;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.x = new org.bondlib.StructBondType.Int32StructField(this, 0, "x", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields((org.bondlib.StructBondType<tests.Base>) getStructType(tests.Base.class), this.x);
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
            this.x.serialize(context, value.x);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
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
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo value) throws java.io.IOException {
            boolean __has_x = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.x = this.x.deserialize(context, field.type);
                        __has_x = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
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

        private java.lang.Object readResolve() throws java.io.ObjectStreamException {
            return getCachedType(this, true);
        }
    }

    public static final org.bondlib.StructBondType<Foo> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

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
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        if (!(super.equals(o))) return false;
        final Foo other = (Foo) o;
        if (!(this.x == other.x)) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += super.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += x;
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}
