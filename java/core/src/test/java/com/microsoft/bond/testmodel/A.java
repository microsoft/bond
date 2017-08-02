// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.testmodel;

import com.microsoft.bond.*;
import com.microsoft.bond.helpers.ArgumentHelper;

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

    // public definition of the type descriptor builder for generic type
    public static abstract class GenericBondTypeBuilder {

        // hide constructor to prevent subclassing outside of the current scope
        private GenericBondTypeBuilder() {
        }

        // public API to make an instance of a generic type
        public abstract <X, Y> StructBondType<A<X, Y>> makeGenericType(BondType<X> X, BondType<Y> Y);
    }

    // private implementation of the type descriptor
    private static final class StructBondTypeImpl<X, Y> extends StructBondType<A<X, Y>> {

        // private implementation of the type descriptor builder
        static final class StructBondTypeBuilderImpl extends StructBondTypeBuilder<A> {

            // called by the public method to make an instance of a generic type
            private <X, Y> StructBondType<A<X, Y>> makeGenericType(BondType<X> X, BondType<Y> Y) {
                ArgumentHelper.ensureNotNull(X, "X");
                ArgumentHelper.ensureNotNull(Y, "Y");
                StructBondType<?> structBondType = this.getInitializedFromCache(X, Y);
                return (StructBondTypeImpl<X, Y>) structBondType;
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 2;
            }

            @Override
            protected final StructBondType<A> buildNewInstance(BondType<?>[] genericTypeArguments) {
                return new StructBondTypeImpl(new GenericTypeSpecialization(genericTypeArguments));
            }

            // registration method
            private static void register() {
                registerStructType(A.class, new StructBondTypeBuilderImpl());
            }
        }

        // field descriptors for each field in the struct
        private ObjectStructField<X> x;
        private ObjectStructField<Y> y;
        private ObjectStructField<X> nx;
        private ObjectStructField<Y> ny;
        private ObjectStructField<B<X>> nbx;
        private ObjectStructField<B<Y>> nby;
        private ObjectStructField<List<X>> lx;
        private ObjectStructField<List<Y>> ly;
        private ObjectStructField<A<X, Integer>> nax32;
        private ObjectStructField<A<Y, Long>> nay64;

        StructBondTypeImpl(GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }

        @Override
        protected final void initialize() {
            BondType<X> X = this.getGenericSpecialization().getGenericTypeArgument(0);
            BondType<Y> Y = this.getGenericSpecialization().getGenericTypeArgument(1);

            // initialize field descriptor
            this.x = new ObjectStructField<X>(
                    this,
                    X,
                    0,
                    "x",
                    Modifier.Optional);

            // initialize field descriptor
            this.y = new ObjectStructField<Y>(
                    this,
                    Y,
                    1,
                    "y",
                    Modifier.Optional);

            // initialize field descriptor
            this.nx = new ObjectStructField<X>(
                    this,
                    nullableOf(X),
                    2,
                    "nx",
                    Modifier.Optional);

            // initialize field descriptor
            this.ny = new ObjectStructField<Y>(
                    this,
                    nullableOf(Y),
                    3,
                    "ny",
                    Modifier.Optional);

            // initialize field descriptor
            this.nbx = new ObjectStructField<B<X>>(
                    this,
                    nullableOf((StructBondType<B<X>>) getStructType(B.class, X)),
                    4,
                    "nbx",
                    Modifier.Optional);

            // initialize field descriptor
            this.nby = new ObjectStructField<B<Y>>(
                    this,
                    nullableOf((StructBondType<B<Y>>) getStructType(B.class, Y)),
                    5,
                    "nby",
                    Modifier.Optional);

            // initialize field descriptor
            this.lx = new ObjectStructField<List<X>>(
                    this,
                    listOf(X),
                    6,
                    "lx",
                    Modifier.Optional);

            // initialize field descriptor
            this.ly = new ObjectStructField<List<Y>>(
                    this,
                    listOf(Y),
                    7,
                    "ly",
                    Modifier.Optional);

            // initialize field descriptor
            this.nax32 = new ObjectStructField<A<X, Integer>>(
                    this,
                    nullableOf((StructBondType<A<X, Integer>>) getStructType(A.class, X, BondTypes.INT32)),
                    8,
                    "nax32",
                    Modifier.Optional);

            // initialize field descriptor
            this.nay64 = new ObjectStructField<A<Y, Long>>(
                    this,
                    nullableOf((StructBondType<A<Y, Long>>) getStructType(A.class, Y, BondTypes.INT64)),
                    9,
                    "nay64",
                    Modifier.Optional);

            // initialize struct descriptor
            super.initializeBaseAndFields(
                    null,
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
        protected final void serializeStructFields(SerializationContext context, A<X, Y> value) throws IOException {
            this.x.serialize(context, value.x);
            this.y.serialize(context, value.y);
            this.nx.serialize(context, value.nx);
            this.ny.serialize(context, value.ny);
            this.nbx.serialize(context, value.nbx);
            this.nby.serialize(context, value.nby);
            this.lx.serialize(context, value.lx);
            this.ly.serialize(context, value.ly);
            this.nax32.serialize(context, value.nax32);
            this.nay64.serialize(context, value.nay64);
        }

        @Override
        protected final void deserializeStructFields(TaggedDeserializationContext context, A<X, Y> value) throws IOException {
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
                        value.x = this.x.deserialize(context, __has_x);
                        __has_x = true;
                        break;
                    case 1:
                        value.y = this.y.deserialize(context, __has_y);
                        __has_y = true;
                        break;
                    case 2:
                        value.nx = this.nx.deserialize(context, __has_nx);
                        __has_nx = true;
                        break;
                    case 3:
                        value.ny = this.ny.deserialize(context, __has_ny);
                        __has_ny = true;
                        break;
                    case 4:
                        value.nbx = this.nbx.deserialize(context, __has_nbx);
                        __has_nbx = true;
                        break;
                    case 5:
                        value.nby = this.nby.deserialize(context, __has_nby);
                        __has_nby = true;
                        break;
                    case 6:
                        value.lx = this.lx.deserialize(context, __has_lx);
                        __has_lx = true;
                        break;
                    case 7:
                        value.ly = this.ly.deserialize(context, __has_ly);
                        __has_ly = true;
                        break;
                    case 8:
                        value.nax32 = this.nax32.deserialize(context, __has_nax32);
                        __has_nax32 = true;
                        break;
                    case 9:
                        value.nay64 = this.nay64.deserialize(context, __has_nay64);
                        __has_nay64 = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }

            this.x.verifyDeserialized(__has_x);
            this.y.verifyDeserialized(__has_y);
            this.nx.verifyDeserialized(__has_nx);
            this.ny.verifyDeserialized(__has_ny);
            this.nbx.verifyDeserialized(__has_nbx);
            this.nby.verifyDeserialized(__has_nby);
            this.lx.verifyDeserialized(__has_lx);
            this.ly.verifyDeserialized(__has_ly);
            this.nax32.verifyDeserialized(__has_nax32);
            this.nay64.verifyDeserialized(__has_nay64);
        }

        @Override
        protected final void deserializeStructFields(UntaggedDeserializationContext context, A<X, Y> value) throws IOException {
        }

        @Override
        public final void initializeStructFields(A<X, Y> value) {
            value.x = this.x.initialize();
            value.y = this.y.initialize();
            value.nx = this.nx.initialize();
            value.ny = this.ny.initialize();
            value.nbx = this.nbx.initialize();
            value.nby = this.nby.initialize();
            value.lx = this.lx.initialize();
            value.ly = this.ly.initialize();
            value.nax32 = this.nax32.initialize();
            value.nay64 = this.nay64.initialize();
        }

        @Override
        protected void cloneStructFields(A<X, Y> fromValue, A<X, Y> toValue) {
            toValue.x = this.x.clone(fromValue.x);
            toValue.y = this.y.clone(fromValue.y);
            toValue.nx = this.nx.clone(fromValue.nx);
            toValue.ny = this.ny.clone(fromValue.ny);
            toValue.nbx = this.nbx.clone(fromValue.nbx);
            toValue.nby = this.nby.clone(fromValue.nby);
            toValue.lx = this.lx.clone(fromValue.lx);
            toValue.ly = this.ly.clone(fromValue.ly);
            toValue.nax32 = this.nax32.clone(fromValue.nax32);
            toValue.nay64 = this.nay64.clone(fromValue.nay64);
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond class static members
    ///////////////////////////////////////////////////////////////////////////

    // builder for type descriptors of this generic struct type
    public static final GenericBondTypeBuilder BOND_TYPE = new GenericBondTypeBuilder() {
        final StructBondTypeImpl.StructBondTypeBuilderImpl builder =
                new StructBondTypeImpl.StructBondTypeBuilderImpl();

        @Override
        public final <X, Y> StructBondType<A<X, Y>> makeGenericType(BondType<X> X, BondType<Y> Y) {
            return this.builder.makeGenericType(X, Y);
        }
    };

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

    // type specialization (added for every generic type)
    private final StructBondTypeImpl<X, Y> __genericType;

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

    // the only constructor which takes the generic type specialization
    public A(StructBondType<A<X, Y>> genericType) {
        super();
        ArgumentHelper.ensureNotNull(genericType, "genericType");
        this.__genericType = (StructBondTypeImpl<X, Y>) genericType;
        this.__genericType.initializeStructFields(this);
    }

    @Override
    public StructBondType<? extends A<X, Y>> getBondType() {
        return this.__genericType;
    }
}
