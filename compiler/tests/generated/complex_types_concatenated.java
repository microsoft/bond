
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

        

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            super.initializeBaseAndFields(null);
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
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo value) throws java.io.IOException {
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
        }
        
        @Override
        protected final void initializeStructFields(Foo value) {
        }
        
        @Override
        protected final void cloneStructFields(Foo fromValue, Foo toValue) {
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
    

    
    
    public Foo() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        
        final Foo other = (Foo) o;
        
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}

package tests;

@javax.annotation.Generated("gbc")
public class ComplexTypes implements org.bondlib.BondSerializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<ComplexTypes> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<ComplexTypes> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<ComplexTypes> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(ComplexTypes.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.ObjectStructField<java.util.List<java.lang.Byte>> li8;

        private org.bondlib.StructBondType.ObjectStructField<java.util.Set<java.lang.Boolean>> sb;

        private org.bondlib.StructBondType.ObjectStructField<java.util.List<org.bondlib.Blob>> vb;

        private org.bondlib.StructBondType.ObjectStructField<java.lang.Float> nf;

        private org.bondlib.StructBondType.ObjectStructField<java.util.Map<java.lang.String, java.lang.String>> msws;

        private org.bondlib.StructBondType.ObjectStructField<org.bondlib.Bonded<tests.Foo>> bfoo;

        private org.bondlib.StructBondType.ObjectStructField<java.util.Map<java.lang.Double, java.util.List<java.util.List<org.bondlib.Bonded<tests.Bar>>>>> m;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.li8 = new org.bondlib.StructBondType.ObjectStructField<java.util.List<java.lang.Byte>>(this, listOf(org.bondlib.BondTypes.INT8), 0, "li8", org.bondlib.Modifier.Optional);
            this.sb = new org.bondlib.StructBondType.ObjectStructField<java.util.Set<java.lang.Boolean>>(this, setOf(org.bondlib.BondTypes.BOOL), 1, "sb", org.bondlib.Modifier.Optional);
            this.vb = new org.bondlib.StructBondType.ObjectStructField<java.util.List<org.bondlib.Blob>>(this, vectorOf(org.bondlib.BondTypes.BLOB), 2, "vb", org.bondlib.Modifier.Optional);
            this.nf = new org.bondlib.StructBondType.ObjectStructField<java.lang.Float>(this, nullableOf(org.bondlib.BondTypes.FLOAT), 3, "nf", org.bondlib.Modifier.Optional);
            this.msws = new org.bondlib.StructBondType.ObjectStructField<java.util.Map<java.lang.String, java.lang.String>>(this, mapOf(org.bondlib.BondTypes.STRING, org.bondlib.BondTypes.WSTRING), 4, "msws", org.bondlib.Modifier.Optional);
            this.bfoo = new org.bondlib.StructBondType.ObjectStructField<org.bondlib.Bonded<tests.Foo>>(this, bondedOf((org.bondlib.StructBondType<tests.Foo>) getStructType(tests.Foo.class)), 5, "bfoo", org.bondlib.Modifier.Optional);
            this.m = new org.bondlib.StructBondType.ObjectStructField<java.util.Map<java.lang.Double, java.util.List<java.util.List<org.bondlib.Bonded<tests.Bar>>>>>(this, mapOf(org.bondlib.BondTypes.DOUBLE, listOf(vectorOf(nullableOf(bondedOf((org.bondlib.StructBondType<tests.Bar>) getStructType(tests.Bar.class)))))), 6, "m", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.li8, this.sb, this.vb, this.nf, this.msws, this.bfoo, this.m);
        }

        @Override
        public final java.lang.String getName() {
            return "ComplexTypes";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "tests.ComplexTypes";
        }

        @Override
        public final java.lang.Class<ComplexTypes> getValueClass() {
            return (java.lang.Class<ComplexTypes>) (java.lang.Class) ComplexTypes.class;
        }

        @Override
        public final ComplexTypes newInstance() {
            return new ComplexTypes();
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, ComplexTypes value) throws java.io.IOException {
            this.li8.serialize(context, value.li8);
            this.sb.serialize(context, value.sb);
            this.vb.serialize(context, value.vb);
            this.nf.serialize(context, value.nf);
            this.msws.serialize(context, value.msws);
            this.bfoo.serialize(context, value.bfoo);
            this.m.serialize(context, value.m);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, ComplexTypes value) throws java.io.IOException {
            boolean __has_li8 = false;
            boolean __has_sb = false;
            boolean __has_vb = false;
            boolean __has_nf = false;
            boolean __has_msws = false;
            boolean __has_bfoo = false;
            boolean __has_m = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.li8 = this.li8.deserialize(context, __has_li8);
                        __has_li8 = true;
                        break;
                    case 1:
                        value.sb = this.sb.deserialize(context, __has_sb);
                        __has_sb = true;
                        break;
                    case 2:
                        value.vb = this.vb.deserialize(context, __has_vb);
                        __has_vb = true;
                        break;
                    case 3:
                        value.nf = this.nf.deserialize(context, __has_nf);
                        __has_nf = true;
                        break;
                    case 4:
                        value.msws = this.msws.deserialize(context, __has_msws);
                        __has_msws = true;
                        break;
                    case 5:
                        value.bfoo = this.bfoo.deserialize(context, __has_bfoo);
                        __has_bfoo = true;
                        break;
                    case 6:
                        value.m = this.m.deserialize(context, __has_m);
                        __has_m = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.li8.verifyDeserialized(__has_li8);
            this.sb.verifyDeserialized(__has_sb);
            this.vb.verifyDeserialized(__has_vb);
            this.nf.verifyDeserialized(__has_nf);
            this.msws.verifyDeserialized(__has_msws);
            this.bfoo.verifyDeserialized(__has_bfoo);
            this.m.verifyDeserialized(__has_m);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, ComplexTypes value) throws java.io.IOException {
            boolean __has_li8 = false;
            boolean __has_sb = false;
            boolean __has_vb = false;
            boolean __has_nf = false;
            boolean __has_msws = false;
            boolean __has_bfoo = false;
            boolean __has_m = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.li8 = this.li8.deserialize(context, field.type);
                        __has_li8 = true;
                        break;
                    case 1:
                        value.sb = this.sb.deserialize(context, field.type);
                        __has_sb = true;
                        break;
                    case 2:
                        value.vb = this.vb.deserialize(context, field.type);
                        __has_vb = true;
                        break;
                    case 3:
                        value.nf = this.nf.deserialize(context, field.type);
                        __has_nf = true;
                        break;
                    case 4:
                        value.msws = this.msws.deserialize(context, field.type);
                        __has_msws = true;
                        break;
                    case 5:
                        value.bfoo = this.bfoo.deserialize(context, field.type);
                        __has_bfoo = true;
                        break;
                    case 6:
                        value.m = this.m.deserialize(context, field.type);
                        __has_m = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.li8.verifyDeserialized(__has_li8);
            this.sb.verifyDeserialized(__has_sb);
            this.vb.verifyDeserialized(__has_vb);
            this.nf.verifyDeserialized(__has_nf);
            this.msws.verifyDeserialized(__has_msws);
            this.bfoo.verifyDeserialized(__has_bfoo);
            this.m.verifyDeserialized(__has_m);
        }
        
        @Override
        protected final void initializeStructFields(ComplexTypes value) {
            value.li8 = this.li8.initialize();
            value.sb = this.sb.initialize();
            value.vb = this.vb.initialize();
            value.nf = this.nf.initialize();
            value.msws = this.msws.initialize();
            value.bfoo = this.bfoo.initialize();
            value.m = this.m.initialize();
        }
        
        @Override
        protected final void cloneStructFields(ComplexTypes fromValue, ComplexTypes toValue) {
            toValue.li8 = this.li8.clone(fromValue.li8);
            toValue.sb = this.sb.clone(fromValue.sb);
            toValue.vb = this.vb.clone(fromValue.vb);
            toValue.nf = this.nf.clone(fromValue.nf);
            toValue.msws = this.msws.clone(fromValue.msws);
            toValue.bfoo = this.bfoo.clone(fromValue.bfoo);
            toValue.m = this.m.clone(fromValue.m);
        }
    }

    public static final org.bondlib.StructBondType<ComplexTypes> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    
    // Java native serialization
    private static final long serialVersionUID = 0L;
    private ComplexTypes __deserializedInstance;

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
    

    public java.util.List<java.lang.Byte> li8;

    public java.util.Set<java.lang.Boolean> sb;

    public java.util.List<org.bondlib.Blob> vb;

    public java.lang.Float nf;

    public java.util.Map<java.lang.String, java.lang.String> msws;

    public org.bondlib.Bonded<tests.Foo> bfoo;

    public java.util.Map<java.lang.Double, java.util.List<java.util.List<org.bondlib.Bonded<tests.Bar>>>> m;
    
    public ComplexTypes() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof ComplexTypes)) return false;
        
        final ComplexTypes other = (ComplexTypes) o;
        if (!((this.li8 == null && other.li8 == null) || (this.li8 != null && this.li8.equals(other.li8)))) return false;
        if (!((this.sb == null && other.sb == null) || (this.sb != null && this.sb.equals(other.sb)))) return false;
        if (!((this.vb == null && other.vb == null) || (this.vb != null && this.vb.equals(other.vb)))) return false;
        if (!((this.nf == null && other.nf == null) || (this.nf != null && this.nf.equals(other.nf)))) return false;
        if (!((this.msws == null && other.msws == null) || (this.msws != null && this.msws.equals(other.msws)))) return false;
        if (!((this.bfoo == null && other.bfoo == null) || (this.bfoo != null && this.bfoo.equals(other.bfoo)))) return false;
        if (!((this.m == null && other.m == null) || (this.m != null && this.m.equals(other.m)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += li8 == null ? 0 : li8.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += sb == null ? 0 : sb.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += vb == null ? 0 : vb.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += nf == null ? 0 : nf.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += msws == null ? 0 : msws.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += bfoo == null ? 0 : bfoo.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += m == null ? 0 : m.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends ComplexTypes> getBondType() {
        return BOND_TYPE;
    }
}
