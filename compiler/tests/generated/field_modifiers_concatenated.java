
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

        private org.bondlib.StructBondType.BoolStructField o;

        private org.bondlib.StructBondType.Int16StructField r;

        private org.bondlib.StructBondType.DoubleStructField ro;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.o = new org.bondlib.StructBondType.BoolStructField(this, 0, "o", org.bondlib.Modifier.Optional);
            this.r = new org.bondlib.StructBondType.Int16StructField(this, 1, "r", org.bondlib.Modifier.Required);
            this.ro = new org.bondlib.StructBondType.DoubleStructField(this, 2, "ro", org.bondlib.Modifier.RequiredOptional);
            super.initializeBaseAndFields(null, this.o, this.r, this.ro);
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
            this.o.serialize(context, value.o);
            this.r.serialize(context, value.r);
            this.ro.serialize(context, value.ro);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
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
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo value) throws java.io.IOException {
            boolean __has_o = false;
            boolean __has_r = false;
            boolean __has_ro = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.o = this.o.deserialize(context, field.type);
                        __has_o = true;
                        break;
                    case 1:
                        value.r = this.r.deserialize(context, field.type);
                        __has_r = true;
                        break;
                    case 2:
                        value.ro = this.ro.deserialize(context, field.type);
                        __has_ro = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.o.verifyDeserialized(__has_o);
            this.r.verifyDeserialized(__has_r);
            this.ro.verifyDeserialized(__has_ro);
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
        if (!(org.bondlib.FloatingPointHelper.doubleEquals(this.ro, other.ro))) return false;
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
        result += org.bondlib.FloatingPointHelper.doubleHashCode(ro);
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}
