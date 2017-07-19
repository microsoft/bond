// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.testmodel;

import com.microsoft.bond.*;

import java.io.IOException;

/**
 * Bond type used for testing, hand-crafted to match generated code.
 *
 <pre>
 struct C : B<double>
 {
     1 : int32 i32;
     2 : int64 i64;
     3 : B<string> bs1;
     4 : B<B<string>> bs2;
     5 : B<B<B<string>>> bs3;
 }
 </pre>
 *
 */
@SuppressWarnings("unchecked")
public class C extends B<Double> implements BondSerializable {

    // private implementation of the type descriptor
    private static final class StructBondTypeImpl extends StructBondType<C> {

        // private implementation of the type descriptor builder
        static final class StructBondTypeBuilderImpl extends StructBondTypeBuilder<C> {

            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final StructBondType<C> buildNewInstance(BondType<?>[] genericTypeArguments) {
                return new StructBondTypeImpl();
            }

            // registration method
            private static void register() {
                registerStructType(C.class, new StructBondTypeBuilderImpl());
            }
        }

        // field descriptors for each field in the struct
        private Int32StructField i32;
        private Int64StructField i64;
        private ObjectStructField<B<String>> bs1;
        private ObjectStructField<B<B<String>>> bs2;
        private ObjectStructField<B<B<B<String>>>> bs3;

        StructBondTypeImpl() {
            super(null);
        }

        @Override
        protected final void initialize() {

            // initialize field descriptor
            this.i32 = new Int32StructField(
                    this,
                    1,
                    "i32",
                    Modifier.Optional,
                    0);

            // initialize field descriptor
            this.i64 = new Int64StructField(
                    this,
                    2,
                    "i64",
                    Modifier.Optional,
                    0L);

            // initialize field descriptor
            this.bs1 = new ObjectStructField<B<String>>(
                    this,
                    (StructBondType<B<String>>) getStructType(B.class, BondTypes.STRING),
                    3,
                    "bs1",
                    Modifier.Optional);

            // initialize field descriptor
            this.bs2 = new ObjectStructField<B<B<String>>>(
                    this,
                    (StructBondType<B<B<String>>>) getStructType(B.class, getStructType(B.class, BondTypes.STRING)),
                    4,
                    "bs2",
                    Modifier.Optional);

            // initialize field descriptor
            this.bs3 = new ObjectStructField<B<B<B<String>>>>(
                    this,
                    (StructBondType<B<B<B<String>>>>) getStructType(B.class, getStructType(B.class, getStructType(B.class, BondTypes.STRING))),
                    5,
                    "bs3",
                    Modifier.Optional);

            // initialize struct descriptor
            super.initializeBaseAndFields(
                    (StructBondType<B<Double>>) getStructType(B.class, BondTypes.DOUBLE),
                    this.i32,
                    this.i64,
                    this.bs1,
                    this.bs2,
                    this.bs3
            );
        }

        @Override
        public final Class<C> getValueClass() {
            return C.class;
        }

        @Override
        public final C newInstance() {
            return new C();
        }

        @Override
        protected final void serializeStructFields(SerializationContext context, C value) throws IOException {
            this.i32.serialize(context, value.i32);
            this.i64.serialize(context, value.i64);
            this.bs1.serialize(context, value.bs1);
            this.bs2.serialize(context, value.bs2);
            this.bs3.serialize(context, value.bs3);
        }

        @Override
        protected final void deserializeStructFields(TaggedDeserializationContext context, C value) throws IOException {
            boolean __has_i32 = false;
            boolean __has_i64 = false;
            boolean __has_bs1 = false;
            boolean __has_bs2 = false;
            boolean __has_bs3 = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.i32 = this.i32.deserialize(context, __has_i32);
                        __has_i32 = true;
                        break;
                    case 2:
                        value.i64 = this.i64.deserialize(context, __has_i64);
                        __has_i64 = true;
                        break;
                    case 3:
                        value.bs1 = this.bs1.deserialize(context, __has_bs1);
                        __has_bs1 = true;
                        break;
                    case 4:
                        value.bs2 = this.bs2.deserialize(context, __has_bs2);
                        __has_bs2 = true;
                        break;
                    case 5:
                        value.bs3 = this.bs3.deserialize(context, __has_bs3);
                        __has_bs3 = true;
                        break;
                }
            }

            this.i32.verifyDeserialized(__has_i32);
            this.i64.verifyDeserialized(__has_i64);
            this.bs1.verifyDeserialized(__has_bs1);
            this.bs2.verifyDeserialized(__has_bs2);
            this.bs3.verifyDeserialized(__has_bs3);
        }

        @Override
        public final void initializeStructFields(C value) {
            value.i32 = this.i32.initialize();
            value.i64 = this.i64.initialize();
            value.bs1 = this.bs1.initialize();
            value.bs2 = this.bs2.initialize();
            value.bs3 = this.bs3.initialize();
        }

        @Override
        protected void cloneStructFields(C fromValue, C toValue) {
            toValue.i32 = this.i32.clone(fromValue.i32);
            toValue.i64 = this.i64.clone(fromValue.i64);
            toValue.bs1 = this.bs1.clone(fromValue.bs1);
            toValue.bs2 = this.bs2.clone(fromValue.bs2);
            toValue.bs3 = this.bs3.clone(fromValue.bs3);
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond class static members
    ///////////////////////////////////////////////////////////////////////////

    // the type descriptor of this struct type
    public static final StructBondType<C> BOND_TYPE =
            new StructBondTypeImpl.StructBondTypeBuilderImpl().getInitializedFromCache();

    // class initialization method (also invoked in static class initializer)
    public static void initializeBondType() {
        StructBondTypeImpl.StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond class instance members
    ///////////////////////////////////////////////////////////////////////////

    // struct fields
    public int i32;
    public long i64;
    public B<String> bs1;
    public B<B<String>> bs2;
    public B<B<B<String>>> bs3;

    // the constructor
    public C() {
        super((StructBondType<B<Double>>) BOND_TYPE.getBaseStructType());
        ((StructBondTypeImpl) BOND_TYPE).initializeStructFields(this);
    }

    @Override
    public StructBondType<? extends BondSerializable> getBondType() {
        return BOND_TYPE;
    }
}
