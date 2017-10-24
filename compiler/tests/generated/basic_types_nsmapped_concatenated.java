
package nsmapped;

@javax.annotation.Generated("gbc")
public class BasicTypes implements org.bondlib.BondSerializable {
    
    private static final class StructBondTypeImpl extends org.bondlib.StructBondType<BasicTypes> {
        
        static final class StructBondTypeBuilderImpl extends org.bondlib.StructBondType.StructBondTypeBuilder<BasicTypes> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final org.bondlib.StructBondType<BasicTypes> buildNewInstance(org.bondlib.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(BasicTypes.class, new StructBondTypeBuilderImpl());
            }
        }

        private org.bondlib.StructBondType.BoolStructField _bool;

        private org.bondlib.StructBondType.StringStructField _str;

        private org.bondlib.StructBondType.WStringStructField _wstr;

        private org.bondlib.StructBondType.UInt64StructField _uint64;

        private org.bondlib.StructBondType.UInt16StructField _uint16;

        private org.bondlib.StructBondType.UInt32StructField _uint32;

        private org.bondlib.StructBondType.UInt8StructField _uint8;

        private org.bondlib.StructBondType.Int8StructField _int8;

        private org.bondlib.StructBondType.Int16StructField _int16;

        private org.bondlib.StructBondType.Int32StructField _int32;

        private org.bondlib.StructBondType.Int64StructField _int64;

        private org.bondlib.StructBondType.DoubleStructField _double;

        private org.bondlib.StructBondType.FloatStructField _float;

        private org.bondlib.StructBondType.ObjectStructField<org.bondlib.Blob> _blob;

        private StructBondTypeImpl(org.bondlib.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this._bool = new org.bondlib.StructBondType.BoolStructField(this, 0, "_bool", org.bondlib.Modifier.Optional);
            this._str = new org.bondlib.StructBondType.StringStructField(this, 2, "_str", org.bondlib.Modifier.Optional);
            this._wstr = new org.bondlib.StructBondType.WStringStructField(this, 3, "_wstr", org.bondlib.Modifier.Optional);
            this._uint64 = new org.bondlib.StructBondType.UInt64StructField(this, 10, "_uint64", org.bondlib.Modifier.Optional);
            this._uint16 = new org.bondlib.StructBondType.UInt16StructField(this, 11, "_uint16", org.bondlib.Modifier.Optional);
            this._uint32 = new org.bondlib.StructBondType.UInt32StructField(this, 12, "_uint32", org.bondlib.Modifier.Optional);
            this._uint8 = new org.bondlib.StructBondType.UInt8StructField(this, 13, "_uint8", org.bondlib.Modifier.Optional);
            this._int8 = new org.bondlib.StructBondType.Int8StructField(this, 14, "_int8", org.bondlib.Modifier.Optional);
            this._int16 = new org.bondlib.StructBondType.Int16StructField(this, 15, "_int16", org.bondlib.Modifier.Optional);
            this._int32 = new org.bondlib.StructBondType.Int32StructField(this, 16, "_int32", org.bondlib.Modifier.Optional);
            this._int64 = new org.bondlib.StructBondType.Int64StructField(this, 17, "_int64", org.bondlib.Modifier.Optional);
            this._double = new org.bondlib.StructBondType.DoubleStructField(this, 18, "_double", org.bondlib.Modifier.Optional);
            this._float = new org.bondlib.StructBondType.FloatStructField(this, 20, "_float", org.bondlib.Modifier.Optional);
            this._blob = new org.bondlib.StructBondType.ObjectStructField<org.bondlib.Blob>(this, org.bondlib.BondTypes.BLOB, 21, "_blob", org.bondlib.Modifier.Optional);
            super.initializeBaseAndFields(null, this._bool, this._str, this._wstr, this._uint64, this._uint16, this._uint32, this._uint8, this._int8, this._int16, this._int32, this._int64, this._double, this._float, this._blob);
        }

        @Override
        public final java.lang.String getName() {
            return "BasicTypes";
        }

        @Override
        public final java.lang.String getQualifiedName() {
            return "tests.BasicTypes";
        }

        @Override
        public final java.lang.Class<BasicTypes> getValueClass() {
            return (java.lang.Class<BasicTypes>) (java.lang.Class) BasicTypes.class;
        }

        @Override
        public final BasicTypes newInstance() {
            return new BasicTypes();
        }
        
        @Override
        protected final void serializeStructFields(org.bondlib.BondType.SerializationContext context, BasicTypes value) throws java.io.IOException {
            this._bool.serialize(context, value._bool);
            this._str.serialize(context, value._str);
            this._wstr.serialize(context, value._wstr);
            this._uint64.serialize(context, value._uint64);
            this._uint16.serialize(context, value._uint16);
            this._uint32.serialize(context, value._uint32);
            this._uint8.serialize(context, value._uint8);
            this._int8.serialize(context, value._int8);
            this._int16.serialize(context, value._int16);
            this._int32.serialize(context, value._int32);
            this._int64.serialize(context, value._int64);
            this._double.serialize(context, value._double);
            this._float.serialize(context, value._float);
            this._blob.serialize(context, value._blob);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.TaggedDeserializationContext context, BasicTypes value) throws java.io.IOException {
            boolean __has__bool = false;
            boolean __has__str = false;
            boolean __has__wstr = false;
            boolean __has__uint64 = false;
            boolean __has__uint16 = false;
            boolean __has__uint32 = false;
            boolean __has__uint8 = false;
            boolean __has__int8 = false;
            boolean __has__int16 = false;
            boolean __has__int32 = false;
            boolean __has__int64 = false;
            boolean __has__double = false;
            boolean __has__float = false;
            boolean __has__blob = false;
            while (this.readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value._bool = this._bool.deserialize(context, __has__bool);
                        __has__bool = true;
                        break;
                    case 2:
                        value._str = this._str.deserialize(context, __has__str);
                        __has__str = true;
                        break;
                    case 3:
                        value._wstr = this._wstr.deserialize(context, __has__wstr);
                        __has__wstr = true;
                        break;
                    case 10:
                        value._uint64 = this._uint64.deserialize(context, __has__uint64);
                        __has__uint64 = true;
                        break;
                    case 11:
                        value._uint16 = this._uint16.deserialize(context, __has__uint16);
                        __has__uint16 = true;
                        break;
                    case 12:
                        value._uint32 = this._uint32.deserialize(context, __has__uint32);
                        __has__uint32 = true;
                        break;
                    case 13:
                        value._uint8 = this._uint8.deserialize(context, __has__uint8);
                        __has__uint8 = true;
                        break;
                    case 14:
                        value._int8 = this._int8.deserialize(context, __has__int8);
                        __has__int8 = true;
                        break;
                    case 15:
                        value._int16 = this._int16.deserialize(context, __has__int16);
                        __has__int16 = true;
                        break;
                    case 16:
                        value._int32 = this._int32.deserialize(context, __has__int32);
                        __has__int32 = true;
                        break;
                    case 17:
                        value._int64 = this._int64.deserialize(context, __has__int64);
                        __has__int64 = true;
                        break;
                    case 18:
                        value._double = this._double.deserialize(context, __has__double);
                        __has__double = true;
                        break;
                    case 20:
                        value._float = this._float.deserialize(context, __has__float);
                        __has__float = true;
                        break;
                    case 21:
                        value._blob = this._blob.deserialize(context, __has__blob);
                        __has__blob = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }
            this._bool.verifyDeserialized(__has__bool);
            this._str.verifyDeserialized(__has__str);
            this._wstr.verifyDeserialized(__has__wstr);
            this._uint64.verifyDeserialized(__has__uint64);
            this._uint16.verifyDeserialized(__has__uint16);
            this._uint32.verifyDeserialized(__has__uint32);
            this._uint8.verifyDeserialized(__has__uint8);
            this._int8.verifyDeserialized(__has__int8);
            this._int16.verifyDeserialized(__has__int16);
            this._int32.verifyDeserialized(__has__int32);
            this._int64.verifyDeserialized(__has__int64);
            this._double.verifyDeserialized(__has__double);
            this._float.verifyDeserialized(__has__float);
            this._blob.verifyDeserialized(__has__blob);
        }
        
        @Override
        protected final void deserializeStructFields(org.bondlib.BondType.UntaggedDeserializationContext context, org.bondlib.StructDef structDef, BasicTypes value) throws java.io.IOException {
            boolean __has__bool = false;
            boolean __has__str = false;
            boolean __has__wstr = false;
            boolean __has__uint64 = false;
            boolean __has__uint16 = false;
            boolean __has__uint32 = false;
            boolean __has__uint8 = false;
            boolean __has__int8 = false;
            boolean __has__int16 = false;
            boolean __has__int32 = false;
            boolean __has__int64 = false;
            boolean __has__double = false;
            boolean __has__float = false;
            boolean __has__blob = false;
            for (final org.bondlib.FieldDef field : structDef.fields) {
                switch (field.id) {
                    case 0:
                        value._bool = this._bool.deserialize(context, field.type);
                        __has__bool = true;
                        break;
                    case 2:
                        value._str = this._str.deserialize(context, field.type);
                        __has__str = true;
                        break;
                    case 3:
                        value._wstr = this._wstr.deserialize(context, field.type);
                        __has__wstr = true;
                        break;
                    case 10:
                        value._uint64 = this._uint64.deserialize(context, field.type);
                        __has__uint64 = true;
                        break;
                    case 11:
                        value._uint16 = this._uint16.deserialize(context, field.type);
                        __has__uint16 = true;
                        break;
                    case 12:
                        value._uint32 = this._uint32.deserialize(context, field.type);
                        __has__uint32 = true;
                        break;
                    case 13:
                        value._uint8 = this._uint8.deserialize(context, field.type);
                        __has__uint8 = true;
                        break;
                    case 14:
                        value._int8 = this._int8.deserialize(context, field.type);
                        __has__int8 = true;
                        break;
                    case 15:
                        value._int16 = this._int16.deserialize(context, field.type);
                        __has__int16 = true;
                        break;
                    case 16:
                        value._int32 = this._int32.deserialize(context, field.type);
                        __has__int32 = true;
                        break;
                    case 17:
                        value._int64 = this._int64.deserialize(context, field.type);
                        __has__int64 = true;
                        break;
                    case 18:
                        value._double = this._double.deserialize(context, field.type);
                        __has__double = true;
                        break;
                    case 20:
                        value._float = this._float.deserialize(context, field.type);
                        __has__float = true;
                        break;
                    case 21:
                        value._blob = this._blob.deserialize(context, field.type);
                        __has__blob = true;
                        break;
                    default:
                        context.reader.skip(context.schema, field.type);
                        break;
                }
            }
            this._bool.verifyDeserialized(__has__bool);
            this._str.verifyDeserialized(__has__str);
            this._wstr.verifyDeserialized(__has__wstr);
            this._uint64.verifyDeserialized(__has__uint64);
            this._uint16.verifyDeserialized(__has__uint16);
            this._uint32.verifyDeserialized(__has__uint32);
            this._uint8.verifyDeserialized(__has__uint8);
            this._int8.verifyDeserialized(__has__int8);
            this._int16.verifyDeserialized(__has__int16);
            this._int32.verifyDeserialized(__has__int32);
            this._int64.verifyDeserialized(__has__int64);
            this._double.verifyDeserialized(__has__double);
            this._float.verifyDeserialized(__has__float);
            this._blob.verifyDeserialized(__has__blob);
        }
        
        @Override
        protected final void initializeStructFields(BasicTypes value) {
            value._bool = this._bool.initialize();
            value._str = this._str.initialize();
            value._wstr = this._wstr.initialize();
            value._uint64 = this._uint64.initialize();
            value._uint16 = this._uint16.initialize();
            value._uint32 = this._uint32.initialize();
            value._uint8 = this._uint8.initialize();
            value._int8 = this._int8.initialize();
            value._int16 = this._int16.initialize();
            value._int32 = this._int32.initialize();
            value._int64 = this._int64.initialize();
            value._double = this._double.initialize();
            value._float = this._float.initialize();
            value._blob = this._blob.initialize();
        }
        
        @Override
        protected final void cloneStructFields(BasicTypes fromValue, BasicTypes toValue) {
            toValue._bool = this._bool.clone(fromValue._bool);
            toValue._str = this._str.clone(fromValue._str);
            toValue._wstr = this._wstr.clone(fromValue._wstr);
            toValue._uint64 = this._uint64.clone(fromValue._uint64);
            toValue._uint16 = this._uint16.clone(fromValue._uint16);
            toValue._uint32 = this._uint32.clone(fromValue._uint32);
            toValue._uint8 = this._uint8.clone(fromValue._uint8);
            toValue._int8 = this._int8.clone(fromValue._int8);
            toValue._int16 = this._int16.clone(fromValue._int16);
            toValue._int32 = this._int32.clone(fromValue._int32);
            toValue._int64 = this._int64.clone(fromValue._int64);
            toValue._double = this._double.clone(fromValue._double);
            toValue._float = this._float.clone(fromValue._float);
            toValue._blob = this._blob.clone(fromValue._blob);
        }
    }

    public static final org.bondlib.StructBondType<BasicTypes> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

    
    // Java native serialization
    private static final long serialVersionUID = 0L;
    private BasicTypes __deserializedInstance;

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
    

    public boolean _bool;

    public java.lang.String _str;

    public java.lang.String _wstr;

    public long _uint64;

    public short _uint16;

    public int _uint32;

    public byte _uint8;

    public byte _int8;

    public short _int16;

    public int _int32;

    public long _int64;

    public double _double;

    public float _float;

    public org.bondlib.Blob _blob;
    
    public BasicTypes() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof BasicTypes)) return false;
        
        final BasicTypes other = (BasicTypes) o;
        if (!(this._bool == other._bool)) return false;
        if (!((this._str == null && other._str == null) || (this._str != null && this._str.equals(other._str)))) return false;
        if (!((this._wstr == null && other._wstr == null) || (this._wstr != null && this._wstr.equals(other._wstr)))) return false;
        if (!(this._uint64 == other._uint64)) return false;
        if (!(this._uint16 == other._uint16)) return false;
        if (!(this._uint32 == other._uint32)) return false;
        if (!(this._uint8 == other._uint8)) return false;
        if (!(this._int8 == other._int8)) return false;
        if (!(this._int16 == other._int16)) return false;
        if (!(this._int32 == other._int32)) return false;
        if (!(this._int64 == other._int64)) return false;
        if (!(org.bondlib.FloatingPointHelper.doubleEquals(this._double, other._double))) return false;
        if (!(org.bondlib.FloatingPointHelper.floatEquals(this._float, other._float))) return false;
        if (!((this._blob == null && other._blob == null) || (this._blob != null && this._blob.equals(other._blob)))) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result += (_bool ? 0 : 1);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _str == null ? 0 : _str.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _wstr == null ? 0 : _wstr.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _uint64 ^ (_uint64 >>> 32);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _uint16;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _uint32;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _uint8;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _int8;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _int16;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _int32;
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _int64 ^ (_int64 >>> 32);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += org.bondlib.FloatingPointHelper.doubleHashCode(_double);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += org.bondlib.FloatingPointHelper.floatHashCode(_float);
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += _blob == null ? 0 : _blob.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

    @Override
    public org.bondlib.StructBondType<? extends BasicTypes> getBondType() {
        return BOND_TYPE;
    }
}
