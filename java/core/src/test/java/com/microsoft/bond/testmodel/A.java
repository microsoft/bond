// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.testmodel;

import com.microsoft.bond.*;
import com.microsoft.bond.helpers.ArgumentHelper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

/**
 * Bond type used for testing, hand-crafted to match generated code.
 *
 <pre>
 struct A<X, Y>
 {
     0 : X x;
     1 : Y y;
     2 : nullable<X> nx;
     3 : nullable<Y> ny;
     4 : nullable<B<X>> nbx;
     5 : nullable<B<Y>> nby;
     6 : list<X> lx;
     7 : list<Y> ly;
     8 : nullable<A<X, int32>> nax32;
     9 : nullable<A<Y, int64>> nay64;
 }
 </pre>
 *
 */
@SuppressWarnings("unchecked")
public class A<X, Y> implements BondSerializable {

    // implementation of the type descriptor resolver
    public static final class StructResolver extends StructBondTypeResolver<A> {

        // register a struct resolver instance so that it can be
        // retrieved by calling static methods of the BondType class
        static {
            registerStructType(A.class, new StructResolver());
        }

        // public type resolver method customized to the generic type parameters
        public final <X, Y> Struct<X, Y> resolve(BondType<X> X, BondType<Y> Y) {
            ArgumentHelper.ensureNotNull(X, "X");
            ArgumentHelper.ensureNotNull(Y, "Y");
            return (Struct<X, Y>) (StructBondType) this.resolveAndInitialize(X, Y);
        }

        @Override
        protected Struct resolveUninitialized(BondType<?>... genericTypeArguments) {
            BondType<?> X = getCachedType(genericTypeArguments[0]);
            BondType<?> Y = getCachedType(genericTypeArguments[1]);
            GenericTypeSpecialization specialization = new GenericTypeSpecialization(X, Y);
            Struct struct = new Struct(specialization);
            return (Struct) getCachedType(struct);
        }
    }

    // implementation of the type descriptor
    public static final class Struct<X, Y> extends StructBondType<A<X, Y>> {

        // field descriptors for each field in the struct
        private StructField<X> x;
        private StructField<Y> y;
        private StructField<X> nx;
        private StructField<Y> ny;
        private StructField<B<X>> nbx;
        private StructField<B<Y>> nby;
        private StructField<List<X>> lx;
        private StructField<List<Y>> ly;
        private StructField<A<X, Integer>> nax32;
        private StructField<A<Y, Long>> nay64;

        // restrict instantiation to the enclosing class and its members
        private Struct(GenericTypeSpecialization specialization) {
            super(Struct.class, null, specialization);
        }

        @Override
        protected final void initialize() {
            BondType<X> __type_X = this.getGenericSpecialization().getGenericTypeArgument(0);
            BondType<Y> __type_Y = this.getGenericSpecialization().getGenericTypeArgument(1);

            // initialize field descriptor
            this.x = new ObjectStructField<X>(
                    this,
                    __type_X,
                    0,
                    "x",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.y = new ObjectStructField<Y>(
                    this,
                    __type_Y,
                    1,
                    "y",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.nx = new ObjectStructField<X>(
                    this,
                    BondType.nullableOf(__type_X),
                    2,
                    "nx",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.ny = new ObjectStructField<Y>(
                    this,
                    BondType.nullableOf(__type_Y),
                    3,
                    "ny",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nbx__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), __type_X);
            this.nbx = new ObjectStructField<B<X>>(
                    this,
                    BondType.nullableOf((StructBondType<B<X>>) __spec_nbx__1),
                    4,
                    "nbx",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nby__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), __type_Y);
            this.nby = new ObjectStructField<B<Y>>(
                    this,
                    BondType.nullableOf((StructBondType<B<Y>>) __spec_nby__1),
                    5,
                    "nby",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.lx = new ObjectStructField<List<X>>(
                    this,
                    BondType.listOf(__type_X),
                    6,
                    "lx",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.ly = new ObjectStructField<List<Y>>(
                    this,
                    BondType.listOf(__type_Y),
                    7,
                    "ly",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nax32__1 =
                    resolveUninitializedWithCaching(new A.StructResolver(), __type_X, BondTypes.INT32);
            this.nax32 = new ObjectStructField<A<X, Integer>>(
                    this,
                    BondType.nullableOf((StructBondType<A<X, Integer>>) __spec_nax32__1),
                    8,
                    "nax32",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nay64__1 =
                    resolveUninitializedWithCaching(new A.StructResolver(), __type_Y, BondTypes.INT64);
            this.nay64 = new ObjectStructField<A<Y, Long>>(
                    this,
                    BondType.nullableOf((StructBondType<A<Y, Long>>) __spec_nay64__1),
                    9,
                    "nay64",
                    Modifier.Optional,
                    false);

            // initialize struct descriptor
            super.initializeFields(
                    this.x,
                    this.y,
                    this.nx,
                    this.ny,
                    this.nbx,
                    this.nby,
                    this.lx,
                    this.ly,
                    this.nax32,
                    this.nay64
            );
        }

        @Override
        public final Class<A<X, Y>> getValueClass() {
            return (Class<A<X, Y>>) (Class<?>) A.class;
        }

        @Override
        public final A<X, Y> newInstance() {
            return new A<X, Y>(this);
        }

        @Override
        protected final void serializeStructFields(
                SerializationContext context, A<X, Y> value) throws IOException {
            this.x.serializeObject(context, value.x);
            this.y.serializeObject(context, value.y);
            this.nx.serializeObject(context, value.nx);
            this.ny.serializeObject(context, value.ny);
            this.nbx.serializeObject(context, value.nbx);
            this.nby.serializeObject(context, value.nby);
            this.lx.serializeObject(context, value.lx);
            this.ly.serializeObject(context, value.ly);
            this.nax32.serializeObject(context, value.nax32);
            this.nay64.serializeObject(context, value.nay64);
        }

        @Override
        protected final void deserializeStructFields(
                TaggedDeserializationContext context, A<X, Y> value) throws IOException {
            boolean __has_x = false;
            boolean __has_y = false;
            boolean __has_nx = false;
            boolean __has_ny = false;
            boolean __has_nbx = false;
            boolean __has_nby = false;
            boolean __has_lx = false;
            boolean __has_ly = false;
            boolean __has_nax32 = false;
            boolean __has_nay64 = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 0:
                        value.x = this.x.deserializeObject(context, __has_x);
                        __has_x = true;
                        break;
                    case 1:
                        value.y = this.y.deserializeObject(context, __has_y);
                        __has_y = true;
                        break;
                    case 2:
                        value.nx = this.nx.deserializeObject(context, __has_nx);
                        __has_nx = true;
                        break;
                    case 3:
                        value.ny = this.ny.deserializeObject(context, __has_ny);
                        __has_ny = true;
                        break;
                    case 4:
                        value.nbx = this.nbx.deserializeObject(context, __has_nbx);
                        __has_nbx = true;
                        break;
                    case 5:
                        value.nby = this.nby.deserializeObject(context, __has_nby);
                        __has_nby = true;
                        break;
                    case 6:
                        value.lx = this.lx.deserializeObject(context, __has_lx);
                        __has_lx = true;
                        break;
                    case 7:
                        value.ly = this.ly.deserializeObject(context, __has_ly);
                        __has_ly = true;
                        break;
                    case 8:
                        value.nax32 = this.nax32.deserializeObject(context, __has_nax32);
                        __has_nax32 = true;
                        break;
                    case 9:
                        value.nay64 = this.nay64.deserializeObject(context, __has_nay64);
                        __has_nay64 = true;
                        break;
                }
            }

            this.x.verifyDeserializedField(__has_x);
            this.y.verifyDeserializedField(__has_y);
            this.nx.verifyDeserializedField(__has_nx);
            this.ny.verifyDeserializedField(__has_ny);
            this.nbx.verifyDeserializedField(__has_nbx);
            this.nby.verifyDeserializedField(__has_nby);
            this.lx.verifyDeserializedField(__has_lx);
            this.ly.verifyDeserializedField(__has_ly);
            this.nax32.verifyDeserializedField(__has_nax32);
            this.nay64.verifyDeserializedField(__has_nay64);
        }

        private void initializeFieldValues(A<X, Y> value) {
            value.x = this.x.initializeObject();
            value.y = this.y.initializeObject();
            value.nx = this.nx.initializeObject();
            value.ny = this.ny.initializeObject();
            value.nbx = this.nbx.initializeObject();
            value.nby = this.nby.initializeObject();
            value.lx = this.lx.initializeObject();
            value.ly = this.ly.initializeObject();
            value.nax32 = this.nax32.initializeObject();
            value.nay64 = this.nay64.initializeObject();
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond class static members
    ///////////////////////////////////////////////////////////////////////////

    // resolver for type descriptors of this generic struct type
    public static final StructResolver struct = new StructResolver();

    ///////////////////////////////////////////////////////////////////////////
    // Bond class instance members
    ///////////////////////////////////////////////////////////////////////////

    // handle to the type specialization
    private final Struct<X, Y> __struct;

    // struct fields
    public X x;
    public Y y;
    public X nx;
    public Y ny;
    public B<X> nbx;
    public B<Y> nby;
    public List<X> lx;
    public List<Y> ly;
    public A<X, Integer> nax32;
    public A<Y, Long> nay64;

    // constructor that takes type specialization (cached)
    public A(Struct<X, Y> struct) {
        ArgumentHelper.ensureNotNull(struct, "struct");
        this.__struct = struct;
        struct.initializeFieldValues(this);
    }

    // constructor that takes individual type parameters
    public A(BondType<X> X, BondType<Y> Y) {
        this(struct.resolve(X, Y));
    }

    @Override
    public StructBondType<? extends BondSerializable> getStruct() {
        return this.__struct;
    }
}
