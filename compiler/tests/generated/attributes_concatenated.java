
package tests;


@javax.annotation.Generated("gbc")
public final class Enum implements org.bondlib.BondEnum<Enum> {

    public static final class Values {
        private Values() {}

        public static final int Value1 = 0;
    }

    private static final class EnumBondTypeImpl extends org.bondlib.EnumBondType<Enum> {

        @Override
        public java.lang.Class<Enum> getValueClass() { return Enum.class; }

        @Override
        public final Enum getEnumValue(int value) { return get(value); }
    }

    public static final org.bondlib.EnumBondType<Enum> BOND_TYPE = new EnumBondTypeImpl();

    public static final Enum Value1 = new Enum(Values.Value1, "Value1");

    public final int value;

    private final java.lang.String label;

    private Enum(int value, java.lang.String label) { this.value = value; this.label = label; }

    @Override
    public final int getValue() { return this.value; }

    @Override
    public final java.lang.String getLabel() { return this.label; }

    @Override
    public final org.bondlib.EnumBondType<Enum> getBondType() { return BOND_TYPE; }

    @Override
    public final int compareTo(Enum o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public final boolean equals(java.lang.Object other) { return (other instanceof Enum) && (this.value == ((Enum) other).value); }

    @Override
    public final int hashCode() { return this.value; }

    @Override
    public final java.lang.String toString() { return this.label != null ? this.label : ("Enum(" + java.lang.String.valueOf(this.value) + ")"); }

    public static Enum get(int value) {
        switch (value) {
            case Values.Value1: return Value1;
            default: return new Enum(value, null);
        }
    }

    public static Enum valueOf(java.lang.String str) {
        if (str == null) {
            throw new java.lang.IllegalArgumentException("Argument 'str' must not be null.");
        } else if (str.equals("Value1")) {
            return Value1;
        } else {
            throw new java.lang.IllegalArgumentException("Invalid 'Enum' enum value: '" + str + "'.");
        }
    }
}

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

        private org.bondlib.StructBondType.StringStructField f;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this.f = new org.bondlib.StructBondType.StringStructField(this, 0, "f", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this.f);
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
            this.f.serialize(context, value.f);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, Foo value) throws java.io.IOException {
            boolean __has_f = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.f = this.f.deserialize(context, __has_f);
                        __has_f = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this.f.verifyDeserialized(__has_f);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, Foo value) throws java.io.IOException {
            boolean __has_f = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value.f = this.f.deserialize(context, field.type);
                        __has_f = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this.f.verifyDeserialized(__has_f);
        }
        
        @Override
        protected final void initializeStructFields(Foo value) {
            value.f = this.f.initialize();
        }
        
        @Override
        protected final void cloneStructFields(Foo fromValue, Foo toValue) {
            toValue.f = this.f.clone(fromValue.f);
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
    

    public java.lang.String f;
    
    public Foo() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Foo)) return false;
        
        final Foo other = (Foo) o;
        if (!((this.f == null && other.f == null) || (this.f != null && this.f.equals(other.f)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += f == null ? 0 : f.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends Foo> getBondType() {
        return BOND_TYPE;
    }
}
