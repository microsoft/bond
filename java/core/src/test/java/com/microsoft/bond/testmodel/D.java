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

    // implementation of the type descriptor
    public static final class Struct extends StructBondType<D> {

        // retrieves singleton, called by the enclosing class
        private static Struct getInstance() {
            Struct instance = (Struct) getCachedType(new Struct());
            instance.ensureInitialized();
            return instance;
        }

        // field descriptors for each field in the struct
        private StructField<A<D, D>> nad;
        private StructField<B<D>> nbd;
        private StructField<C> nc;
        private StructField<D> nd;
        private StructField<E<D>> ned;

        // restrict instantiation to the enclosing class and its members
        public Struct() {
            super(Struct.class, (C.Struct) getCachedType(new C.Struct()), null);
        }

        @Override
        protected final void initialize() {

            // initialize field descriptor
            StructBondType __spec_nad__1 =
                    resolveUninitializedWithCaching(new A.StructResolver(), new D.Struct(), new D.Struct());
            this.nad = new ObjectStructField<A<D, D>>(
                    this,
                    BondType.nullableOf((StructBondType<A<D, D>>) __spec_nad__1),
                    1,
                    "nad",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nbd__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), new D.Struct());
            this.nbd = new ObjectStructField<B<D>>(
                    this,
                    BondType.nullableOf((StructBondType<B<D>>) __spec_nbd__1),
                    2,
                    "nbd",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.nc = new ObjectStructField<C>(
                    this,
                    BondType.nullableOf(new C.Struct()),
                    3,
                    "nc",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.nd = new ObjectStructField<D>(
                    this,
                    BondType.nullableOf(new D.Struct()),
                    4,
                    "nd",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_ned__1 =
                    resolveUninitializedWithCaching(new E.StructResolver(), new D.Struct());
            this.ned = new ObjectStructField<E<D>>(
                    this,
                    BondType.nullableOf((StructBondType<E<D>>) __spec_ned__1),
                    5,
                    "ned",
                    Modifier.Optional,
                    false);

            // initialize struct descriptor
            super.initializeFields(
                    this.nad,
                    this.nbd,
                    this.nc,
                    this.nd,
                    this.ned
            );
        }

        @Override
        public final Class<D> getValueClass() {
            Class clazz = D.class;
            return (Class<D>) clazz;
        }

        @Override
        public final D newInstance() {
            return new D();
        }

        @Override
        protected final void serializeStructFields(
                SerializationContext context, D value) throws IOException {
            this.nad.serializeObject(context, value.nad);
            this.nbd.serializeObject(context, value.nbd);
            this.nc.serializeObject(context, value.nc);
            this.nd.serializeObject(context, value.nd);
            this.ned.serializeObject(context, value.ned);
        }

        @Override
        protected final void deserializeStructFields(
                TaggedDeserializationContext context, D value) throws IOException {
            boolean __has_nad = false;
            boolean __has_nbd = false;
            boolean __has_nc = false;
            boolean __has_nd = false;
            boolean __has_ned = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.nad = this.nad.deserializeObject(context, __has_nad);
                        __has_nad = true;
                        break;
                    case 2:
                        value.nbd = this.nbd.deserializeObject(context, __has_nbd);
                        __has_nbd = true;
                        break;
                    case 3:
                        value.nc = this.nc.deserializeObject(context, __has_nc);
                        __has_nc = true;
                        break;
                    case 4:
                        value.nd = this.nd.deserializeObject(context, __has_nd);
                        __has_nd = true;
                        break;
                    case 5:
                        value.ned = this.ned.deserializeObject(context, __has_ned);
                        __has_ned = true;
                        break;
                }
            }

            this.nad.verifyDeserializedField(__has_nad);
            this.nbd.verifyDeserializedField(__has_nbd);
            this.nc.verifyDeserializedField(__has_nc);
            this.nd.verifyDeserializedField(__has_nd);
            this.ned.verifyDeserializedField(__has_ned);
        }

        private void initializeFieldValues(D value) {
            value.nad = this.nad.initializeObject();
            value.nbd = this.nbd.initializeObject();
            value.nc = this.nc.initializeObject();
            value.nd = this.nd.initializeObject();
            value.ned = this.ned.initializeObject();
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
    public A<D, D> nad;
    public B<D> nbd;
    public C nc;
    public D nd;
    public E<D> ned;

    // the parameterless constructor
    public D() {
        super();
        struct.initializeFieldValues(this);
    }

    @Override
    public StructBondType<? extends BondSerializable> getStruct() {
        return struct;
    }
}
