
package import_test;

@javax.annotation.Generated("gbc")
public class HasEmpty implements org.bondlib.BondSerializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<HasEmpty> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<HasEmpty> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<HasEmpty> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(HasEmpty.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.ObjectStructField<empty.Empty> e;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.e = new org.bondlib.StructBondType.ObjectStructField<empty.Empty>(this, (org.bondlib.StructBondType<empty.Empty>) getStructType(empty.Empty.class), 0, "e", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.e);
        }

        @Override
        public final java.lang.String getName() {
            return "HasEmpty";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "import_test.HasEmpty";
        }

        @Override
        public final java.lang.Class<HasEmpty> getValueClass() {
            return (java.lang.Class<HasEmpty>) (java.lang.Class) HasEmpty.class;
        }

        @Override
        public final HasEmpty newInstance() {
            return new HasEmpty();
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, HasEmpty value) throws java.io.IOException {
            this.e.serialize(context, value.e);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, HasEmpty value) throws java.io.IOException {
            boolean __has_e = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.e = this.e.deserialize(context, __has_e);
                        __has_e = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.e.verifyDeserialized(__has_e);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, HasEmpty value) throws java.io.IOException {
            boolean __has_e = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.e = this.e.deserialize(context, field.type);
                        __has_e = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.e.verifyDeserialized(__has_e);
        }
        
        @Override
        protected final void initializeStructFields(HasEmpty value) {
            value.e = this.e.initialize();
        }
        
        @Override
        protected final void cloneStructFields(HasEmpty fromValue, HasEmpty toValue) {
            toValue.e = this.e.clone(fromValue.e);
        }
    }

    public static final org.bondlib.StructBondType<HasEmpty> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    
    // Java native serialization
    private static final long serialVersionUID = 0L;
    private HasEmpty __deserializedInstance;

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
    

    public empty.Empty e;
    
    public HasEmpty() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof HasEmpty)) return false;
        
        final HasEmpty other = (HasEmpty) o;
        if (!((this.e == null && other.e == null) || (this.e != null && this.e.equals(other.e)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += e == null ? 0 : e.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends HasEmpty> getBondType() {
        return BOND_TYPE;
    }
}
