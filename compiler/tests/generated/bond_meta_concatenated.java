
package deprecated.bondmeta;

@javax.annotation.Generated("gbc")
public class HasMetaFields implements org.bondlib.BondSerializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<HasMetaFields> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<HasMetaFields> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<HasMetaFields> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(HasMetaFields.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.StringStructField full_name;

        private org.bondlib.StructBondType.StringStructField name;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.full_name = new org.bondlib.StructBondType.StringStructField(this, 0, "full_name", org.bondlib.Modifier.Optional);
            this.name = new org.bondlib.StructBondType.StringStructField(this, 1, "name", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.full_name, this.name);
        }

        @Override
        public final java.lang.String getName() {
            return "HasMetaFields";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "deprecated.bondmeta.HasMetaFields";
        }

        @Override
        public final java.lang.Class<HasMetaFields> getValueClass() {
            return (java.lang.Class<HasMetaFields>) (java.lang.Class) HasMetaFields.class;
        }

        @Override
        public final HasMetaFields newInstance() {
            return new HasMetaFields();
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, HasMetaFields value) throws java.io.IOException {
            this.full_name.serialize(context, value.full_name);
            this.name.serialize(context, value.name);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, HasMetaFields value) throws java.io.IOException {
            boolean __has_full_name = false;
            boolean __has_name = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.full_name = this.full_name.deserialize(context, __has_full_name);
                        __has_full_name = true;
                        break;
                    case 1:
                        value.name = this.name.deserialize(context, __has_name);
                        __has_name = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.full_name.verifyDeserialized(__has_full_name);
            this.name.verifyDeserialized(__has_name);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, HasMetaFields value) throws java.io.IOException {
            boolean __has_full_name = false;
            boolean __has_name = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.full_name = this.full_name.deserialize(context, field.type);
                        __has_full_name = true;
                        break;
                    case 1:
                        value.name = this.name.deserialize(context, field.type);
                        __has_name = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.full_name.verifyDeserialized(__has_full_name);
            this.name.verifyDeserialized(__has_name);
        }
        
        @Override
        protected final void initializeStructFields(HasMetaFields value) {
            value.full_name = this.full_name.initialize();
            value.name = this.name.initialize();
        }
        
        @Override
        protected final void cloneStructFields(HasMetaFields fromValue, HasMetaFields toValue) {
            toValue.full_name = this.full_name.clone(fromValue.full_name);
            toValue.name = this.name.clone(fromValue.name);
        }
    }

    public static final org.bondlib.StructBondType<HasMetaFields> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    
    // Java native serialization
    private static final long serialVersionUID = 0L;
    private HasMetaFields __deserializedInstance;

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
    

    public java.lang.String full_name;

    public java.lang.String name;
    
    public HasMetaFields() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof HasMetaFields)) return false;
        
        final HasMetaFields other = (HasMetaFields) o;
        if (!((this.full_name == null && other.full_name == null) || (this.full_name != null && this.full_name.equals(other.full_name)))) return false;
        if (!((this.name == null && other.name == null) || (this.name != null && this.name.equals(other.name)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += full_name == null ? 0 : full_name.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += name == null ? 0 : name.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends HasMetaFields> getBondType() {
        return BOND_TYPE;
    }
}
