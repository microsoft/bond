// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.testmodel;

import com.microsoft.bond.*;

import java.io.IOException;

/**
 * Bond type used for testing, hand-crafted to match generated code.
 *
 <pre>
 struct D : C
 {
     1 : nullable<A<D, D>> nad;
     2 : nullable<B<D>> nbd;
     3 : nullable<C> nc;
     4 : nullable<D> nd;
     5 : nullable<E<D>> ned;
 }
 </pre>
 *
 */
@SuppressWarnings("unchecked")
public class D extends C implements BondSerializable {

    // private implementation of the type descriptor
    private static final class StructBondTypeImpl extends StructBondType<D> {

        // private implementation of the type descriptor builder
        static final class StructBondTypeBuilderImpl extends StructBondTypeBuilder<D> {

            @Override
            public final int getGenericTypeParameterCount() {
                return 0;
            }

            @Override
            protected final StructBondType<D> buildNewInstance(BondType<?>[] genericTypeArguments) {
                return new StructBondTypeImpl();
            }

            // registration method
            private static void register() {
                registerStructType(D.class, new StructBondTypeBuilderImpl());
            }
        }

        // field descriptors for each field in the struct
        private ObjectStructField<A<D, D>> nad;
        private ObjectStructField<B<D>> nbd;
        private ObjectStructField<C> nc;
        private ObjectStructField<D> nd;
        private ObjectStructField<E<D>> ned;

        StructBondTypeImpl() {
            super(null);
        }

        @Override
        protected final void initialize() {

            // initialize field descriptor
            this.nad = new ObjectStructField<A<D, D>>(
                    this,
                    nullableOf((StructBondType<A<D, D>>) getStructType(A.class, getStructType(D.class), getStructType(D.class))),
                    1,
                    "nad",
                    Modifier.Optional);

            // initialize field descriptor
            this.nbd = new ObjectStructField<B<D>>(
                    this,
                    nullableOf((StructBondType<B<D>>) getStructType(B.class, getStructType(D.class))),
                    2,
                    "nbd",
                    Modifier.Optional);

            // initialize field descriptor
            this.nc = new ObjectStructField<C>(
                    this,
                    nullableOf((StructBondType<C>) getStructType(C.class)),
                    3,
                    "nc",
                    Modifier.Optional);

            // initialize field descriptor
            this.nd = new ObjectStructField<D>(
                    this,
                    nullableOf((StructBondType<D>) getStructType(D.class)),
                    4,
                    "nd",
                    Modifier.Optional);

            // initialize field descriptor
            this.ned = new ObjectStructField<E<D>>(
                    this,
                    nullableOf((StructBondType<E<D>>) getStructType(E.class, getStructType(D.class))),
                    5,
                    "ned",
                    Modifier.Optional);

            // initialize struct descriptor
            super.initializeBaseAndFields(
                    (StructBondType<C>) getStructType(C.class),
                    this.nad,
                    this.nbd,
                    this.nc,
                    this.nd,
                    this.ned
            );
        }

        @Override
        public final Class<D> getValueClass() {
            return D.class;
        }

        @Override
        public final D newInstance() {
            return new D();
        }

        @Override
        protected final void serializeStructFields(SerializationContext context, D value) throws IOException {
            this.nad.serialize(context, value.nad);
            this.nbd.serialize(context, value.nbd);
            this.nc.serialize(context, value.nc);
            this.nd.serialize(context, value.nd);
            this.ned.serialize(context, value.ned);
        }

        @Override
        protected final void deserializeStructFields(TaggedDeserializationContext context, D value) throws IOException {
            boolean __has_nad = false;
            boolean __has_nbd = false;
            boolean __has_nc = false;
            boolean __has_nd = false;
            boolean __has_ned = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.nad = this.nad.deserialize(context, __has_nad);
                        __has_nad = true;
                        break;
                    case 2:
                        value.nbd = this.nbd.deserialize(context, __has_nbd);
                        __has_nbd = true;
                        break;
                    case 3:
                        value.nc = this.nc.deserialize(context, __has_nc);
                        __has_nc = true;
                        break;
                    case 4:
                        value.nd = this.nd.deserialize(context, __has_nd);
                        __has_nd = true;
                        break;
                    case 5:
                        value.ned = this.ned.deserialize(context, __has_ned);
                        __has_ned = true;
                        break;
                }
            }

            this.nad.verifyDeserialized(__has_nad);
            this.nbd.verifyDeserialized(__has_nbd);
            this.nc.verifyDeserialized(__has_nc);
            this.nd.verifyDeserialized(__has_nd);
            this.ned.verifyDeserialized(__has_ned);
        }

        @Override
        public final void initializeStructFields(D value) {
            value.nad = this.nad.initialize();
            value.nbd = this.nbd.initialize();
            value.nc = this.nc.initialize();
            value.nd = this.nd.initialize();
            value.ned = this.ned.initialize();
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond class static members
    ///////////////////////////////////////////////////////////////////////////

    // the type descriptor of this struct type
    public static final StructBondType<D> BOND_TYPE =
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
    public A<D, D> nad;
    public B<D> nbd;
    public C nc;
    public D nd;
    public E<D> ned;

    // the constructor
    public D() {
        super();
        ((StructBondTypeImpl) BOND_TYPE).initializeStructFields(this);
    }

    @Override
    public StructBondType<? extends BondSerializable> getBondType() {
        return BOND_TYPE;
    }
}
