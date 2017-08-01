
package tests;

@javax.annotation.Generated("gbc")
public class BasicTypes implements com.microsoft.bond.BondSerializable {
    
    private static final class StructBondTypeImpl extends com.microsoft.bond.StructBondType<BasicTypes> {
        
        static final class StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<BasicTypes> {
            
            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final com.microsoft.bond.StructBondType<BasicTypes> buildNewInstance(com.microsoft.bond.BondType[] genericTypeArguments) {
                return new StructBondTypeImpl(null);
            }

            static void register() {
                registerStructType(BasicTypes.class, new StructBondTypeBuilderImpl());
            }
        }

        private com.microsoft.bond.StructBondType.BoolStructField _bool;

        private com.microsoft.bond.StructBondType.StringStructField _str;

        private com.microsoft.bond.StructBondType.WStringStructField _wstr;

        private com.microsoft.bond.StructBondType.UInt64StructField _uint64;

        private com.microsoft.bond.StructBondType.UInt16StructField _uint16;

        private com.microsoft.bond.StructBondType.UInt32StructField _uint32;

        private com.microsoft.bond.StructBondType.UInt8StructField _uint8;

        private com.microsoft.bond.StructBondType.Int8StructField _int8;

        private com.microsoft.bond.StructBondType.Int16StructField _int16;

        private com.microsoft.bond.StructBondType.Int32StructField _int32;

        private com.microsoft.bond.StructBondType.Int64StructField _int64;

        private com.microsoft.bond.StructBondType.DoubleStructField _double;

        private com.microsoft.bond.StructBondType.FloatStructField _float;

        private com.microsoft.bond.StructBondType.ObjectStructField<byte[]> _blob;

        private StructBondTypeImpl(com.microsoft.bond.GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }
        
        @Override
        protected final void initialize() {
            this._bool = new com.microsoft.bond.StructBondType.BoolStructField(this, 0, "_bool", com.microsoft.bond.Modifier.Optional);
            this._str = new com.microsoft.bond.StructBondType.StringStructField(this, 2, "_str", com.microsoft.bond.Modifier.Optional);
            this._wstr = new com.microsoft.bond.StructBondType.WStringStructField(this, 3, "_wstr", com.microsoft.bond.Modifier.Optional);
            this._uint64 = new com.microsoft.bond.StructBondType.UInt64StructField(this, 10, "_uint64", com.microsoft.bond.Modifier.Optional);
            this._uint16 = new com.microsoft.bond.StructBondType.UInt16StructField(this, 11, "_uint16", com.microsoft.bond.Modifier.Optional);
            this._uint32 = new com.microsoft.bond.StructBondType.UInt32StructField(this, 12, "_uint32", com.microsoft.bond.Modifier.Optional);
            this._uint8 = new com.microsoft.bond.StructBondType.UInt8StructField(this, 13, "_uint8", com.microsoft.bond.Modifier.Optional);
            this._int8 = new com.microsoft.bond.StructBondType.Int8StructField(this, 14, "_int8", com.microsoft.bond.Modifier.Optional);
            this._int16 = new com.microsoft.bond.StructBondType.Int16StructField(this, 15, "_int16", com.microsoft.bond.Modifier.Optional);
            this._int32 = new com.microsoft.bond.StructBondType.Int32StructField(this, 16, "_int32", com.microsoft.bond.Modifier.Optional);
            this._int64 = new com.microsoft.bond.StructBondType.Int64StructField(this, 17, "_int64", com.microsoft.bond.Modifier.Optional);
            this._double = new com.microsoft.bond.StructBondType.DoubleStructField(this, 18, "_double", com.microsoft.bond.Modifier.Optional);
            this._float = new com.microsoft.bond.StructBondType.FloatStructField(this, 20, "_float", com.microsoft.bond.Modifier.Optional);
            this._blob = new com.microsoft.bond.StructBondType.ObjectStructField<byte[]>(this, com.microsoft.bond.BondTypes.BLOB, 21, "_blob", com.microsoft.bond.Modifier.Optional);
            super.initializeBaseAndFields(null, this._bool, this._str, this._wstr, this._uint64, this._uint16, this._uint32, this._uint8, this._int8, this._int16, this._int32, this._int64, this._double, this._float, this._blob);
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
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, BasicTypes value) throws java.io.IOException {
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
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, BasicTypes value) throws java.io.IOException {
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
        protected final void deserializeStructFields(com.microsoft.bond.BondType.UntaggedDeserializationContext context, BasicTypes value) throws java.io.IOException {
            this._bool.deserialize(context);
            this._str.deserialize(context);
            this._wstr.deserialize(context);
            this._uint64.deserialize(context);
            this._uint16.deserialize(context);
            this._uint32.deserialize(context);
            this._uint8.deserialize(context);
            this._int8.deserialize(context);
            this._int16.deserialize(context);
            this._int32.deserialize(context);
            this._int64.deserialize(context);
            this._double.deserialize(context);
            this._float.deserialize(context);
            this._blob.deserialize(context);
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

    public static final com.microsoft.bond.StructBondType<BasicTypes> BOND_TYPE = new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    

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

    public byte[] _blob;
    
    public BasicTypes() {
        super();
        ((StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };


    @Override
    public com.microsoft.bond.StructBondType<? extends BasicTypes> getBondType() {
        return BOND_TYPE;
    }
}
