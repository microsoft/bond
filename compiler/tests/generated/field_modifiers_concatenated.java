
package tests;

@javax.annotation.Generated("gbc")
public class Foo implements com.microsoft.bond.BondSerializable {
    
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

        private com.microsoft.bond.StructBondType.BoolStructField o;

        private com.microsoft.bond.StructBondType.Int16StructField r;

        private com.microsoft.bond.StructBondType.DoubleStructField ro;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.o = new com.microsoft.bond.StructBondType.BoolStructField(this, 0, "o", com.microsoft.bond.Modifier.Optional);
            this.r = new com.microsoft.bond.StructBondType.Int16StructField(this, 1, "r", com.microsoft.bond.Modifier.Required);
            this.ro = new com.microsoft.bond.StructBondType.DoubleStructField(this, 2, "ro", com.microsoft.bond.Modifier.RequiredOptional);
            super.initializeBaseAndFields(null, this.o, this.r, this.ro);
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
            this.o.serialize(context, value.o);
            this.r.serialize(context, value.r);
            this.ro.serialize(context, value.ro);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
            boolean __has_o = false;
            boolean __has_r = false;
            boolean __has_ro = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.o = this.o.deserialize(context, __has_o);
                        __has_o = true;
                        break;
                    case 1:
                        value.r = this.r.deserialize(context, __has_r);
                        __has_r = true;
                        break;
                    case 2:
                        value.ro = this.ro.deserialize(context, __has_ro);
                        __has_ro = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.o.verifyDeserialized(__has_o);
            this.r.verifyDeserialized(__has_r);
            this.ro.verifyDeserialized(__has_ro);
        }
        
        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.UntaggedDeserializationContext context, Foo value) throws java.io.IOException {
            value.o = this.o.deserialize(context);
            value.r = this.r.deserialize(context);
            value.ro = this.ro.deserialize(context);
        }
        
        @Override
        protected final void initializeStructFields(Foo value) {
            value.o = this.o.initialize();
            value.r = this.r.initialize();
            value.ro = this.ro.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Foo fromValue, Foo toValue) {
            toValue.o = this.o.clone(fromValue.o);
            toValue.r = this.r.clone(fromValue.r);
            toValue.ro = this.ro.clone(fromValue.ro);
        }
    }

    public static final com.microsoft.bond.StructBondType<Foo> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    public boolean o;

    public short r;

    public double ro;
    
    public Foo() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        
        final Foo other = (Foo) o;
        if (!(this.o == other.o)) return false;
        if (!(this.r == other.r)) return false;
        if (!(com.microsoft.bond.helpers.FloatingPointHelper.doubleEquals(this.ro, other.ro))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += (o ? 0 : 1);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += r;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += com.microsoft.bond.helpers.FloatingPointHelper.doubleHashCode(ro);
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public com.microsoft.bond.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}
