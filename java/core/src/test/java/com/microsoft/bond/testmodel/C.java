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

    // implementation of the type descriptor resolver (private since the class is not generic)
    private static final class StructResolver extends StructBondTypeResolver<C> {

        // register a struct resolver instance so that it can be
        // retrieved by calling static methods of the BondType class
        static {
            registerStructType(C.class, new StructResolver());
        }

        @Override
        protected StructBondType<C> resolveUninitialized(BondType<?>[] genericTypeArguments) {
            return (Struct) getCachedType(new Struct());
        }
    }

    // implementation of the type descriptor
    public static final class Struct extends StructBondType<C> {

        // retrieves singleton, called by the enclosing class
        private static Struct getInstance() {
            Struct instance = (Struct) getCachedType(new Struct());
            instance.ensureInitialized();
            return instance;
        }

        // field descriptors for each field in the struct
        private Int32StructField i32;
        private Int64StructField i64;
        private StructField<B<String>> bs1;
        private StructField<B<B<String>>> bs2;
        private StructField<B<B<B<String>>>> bs3;

        // restrict instantiation to the enclosing class and its members
        public Struct() {
            super(Struct.class, resolveUninitializedWithCaching(new B.StructResolver(), BondTypes.DOUBLE), null);
        }

        @Override
        protected final void initialize() {

            // initialize field descriptor
            this.i32 = new Int32StructField(
                    this,
                    1,
                    "i32",
                    Modifier.Optional,
                    false,
                    0);

            // initialize field descriptor
            this.i64 = new Int64StructField(
                    this,
                    2,
                    "i64",
                    Modifier.Optional,
                    false,
                    0L);

            // initialize field descriptor
            StructBondType __spec_bs1__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), BondTypes.STRING);
            this.bs1 = new ObjectStructField<B<String>>(
                    this,
                    (StructBondType<B<String>>) __spec_bs1__1,
                    3,
                    "bs1",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_bs2__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), BondTypes.STRING);
            StructBondType __spec_bs2__2 =
                    resolveUninitializedWithCaching(new B.StructResolver(), __spec_bs2__1);
            this.bs2 = new ObjectStructField<B<B<String>>>(
                    this,
                    (StructBondType<B<B<String>>>) __spec_bs2__2,
                    4,
                    "bs2",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_bs3__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), BondTypes.STRING);
            StructBondType __spec_bs3__2 =
                    resolveUninitializedWithCaching(new B.StructResolver(), __spec_bs3__1);
            StructBondType __spec_bs3__3 =
                    resolveUninitializedWithCaching(new B.StructResolver(), __spec_bs3__2);
            this.bs3 = new ObjectStructField<B<B<B<String>>>>(
                    this,
                    (StructBondType<B<B<B<String>>>>) __spec_bs3__3,
                    5,
                    "bs3",
                    Modifier.Optional,
                    false);

            // initialize struct descriptor
            super.initializeFields(
                    this.i32,
                    this.i64,
                    this.bs1,
                    this.bs2,
                    this.bs3
            );
        }

        @Override
        public final Class<C> getValueClass() {
            return (Class<C>) (Class<?>) C.class;
        }

        @Override
        public final C newInstance() {
            return new C();
        }

        @Override
        protected final void serializeStructFields(
                SerializationContext context, C value) throws IOException {
            this.i32.serializeInt32(context, value.i32);
            this.i64.serializeInt64(context, value.i64);
            this.bs1.serializeObject(context, value.bs1);
            this.bs2.serializeObject(context, value.bs2);
            this.bs3.serializeObject(context, value.bs3);
        }

        @Override
        protected final void deserializeStructFields(
                TaggedDeserializationContext context, C value) throws IOException {
            boolean __has_i32 = false;
            boolean __has_i64 = false;
            boolean __has_bs1 = false;
            boolean __has_bs2 = false;
            boolean __has_bs3 = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.i32 = this.i32.deserializeInt32(context, __has_i32);
                        __has_i32 = true;
                        break;
                    case 2:
                        value.i64 = this.i64.deserializeInt64(context, __has_i64);
                        __has_i64 = true;
                        break;
                    case 3:
                        value.bs1 = this.bs1.deserializeObject(context, __has_bs1);
                        __has_bs1 = true;
                        break;
                    case 4:
                        value.bs2 = this.bs2.deserializeObject(context, __has_bs2);
                        __has_bs2 = true;
                        break;
                    case 5:
                        value.bs3 = this.bs3.deserializeObject(context, __has_bs3);
                        __has_bs3 = true;
                        break;
                }
            }

            this.i32.verifyDeserializedField(__has_i32);
            this.i64.verifyDeserializedField(__has_i64);
            this.bs1.verifyDeserializedField(__has_bs1);
            this.bs2.verifyDeserializedField(__has_bs2);
            this.bs3.verifyDeserializedField(__has_bs3);
        }

        private void initializeFieldValues(C value) {
            value.i32 = this.i32.initializeInt32();
            value.i64 = this.i64.initializeInt64();
            value.bs1 = this.bs1.initializeObject();
            value.bs2 = this.bs2.initializeObject();
            value.bs3 = this.bs3.initializeObject();
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond class static members
    ///////////////////////////////////////////////////////////////////////////

    // type descriptors of this struct type
    public static final Struct struct = Struct.getInstance();

    ///////////////////////////////////////////////////////////////////////////
    // Bond class instance members
    ///////////////////////////////////////////////////////////////////////////

    // struct fields
    public int i32;
    public long i64;
    public B<String> bs1;
    public B<B<String>> bs2;
    public B<B<B<String>>> bs3;

    // the parameterless constructor
    public C() {
        super((B.Struct<Double>) (B.Struct) struct.getBaseStructType());
        struct.initializeFieldValues(this);
    }
}
