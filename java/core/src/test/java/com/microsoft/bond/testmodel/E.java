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
 struct E<U> : D
 {
     1 : U u;
     2 : nullable<E<U>> neu;
     3 : list<nullable<E<U>>> lneu;
     4 : nullable<list<nullable<E<U>>>> nlneu;
 }
 </pre>
 *
 */
@SuppressWarnings("unchecked")
public class E<U> extends D implements BondSerializable {

    // implementation of the type descriptor resolver
    public static final class StructResolver extends StructBondTypeResolver<E> {

        // public type resolver method customized to the generic type parameters
        public final <U> Struct<U> resolve(BondType<U> U) {
            ArgumentHelper.ensureNotNull(U, "U");
            Struct<U> type = (Struct<U>) this.resolveUninitialized(U);
            ensureInitialized(type);
            return type;
        }

        @Override
        protected Struct resolveUninitialized(BondType<?>... genericTypeArguments) {
            BondType<?> U = getCachedType(genericTypeArguments[0]);
            GenericTypeSpecialization specialization = new GenericTypeSpecialization(U);
            StructBondType<? super E> base = (D.Struct) getCachedType(new D.Struct());
            Struct struct = new Struct(base, specialization);
            return (Struct) getCachedType(struct);
        }
    }

    // implementation of the type descriptor
    public static final class Struct<U> extends StructBondType<E<U>> {

        // field descriptors for each field in the struct
        private StructField<U> u;
        private StructField<E<U>> neu;
        private StructField<List<E<U>>> lneu;
        private StructField<List<E<U>>> nlneu;

        // restrict instantiation to the enclosing class and its members
        private Struct(StructBondType<? super E> base, GenericTypeSpecialization specialization) {
            super(Struct.class, base, specialization);
        }

        @Override
        protected final void initialize() {
            BondType<U> __type_U = this.getGenericSpecialization().getGenericTypeArgument(0);

            // initialize field descriptor
            this.u = new ObjectStructField<U>(
                    this,
                    __type_U,
                    1,
                    "u",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_neu__1 =
                    resolveUninitializedWithCaching(new E.StructResolver(), __type_U);
            this.neu = new ObjectStructField<E<U>>(
                    this,
                    BondType.nullableOf((StructBondType<E<U>>) __spec_neu__1),
                    2,
                    "neu",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_lneu__1 =
                    resolveUninitializedWithCaching(new E.StructResolver(), __type_U);
            this.lneu = new ObjectStructField<List<E<U>>>(
                    this,
                    BondType.listOf(
                            BondType.nullableOf((StructBondType<E<U>>) __spec_neu__1)),
                    3,
                    "lneu",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nlneu__1 =
                    resolveUninitializedWithCaching(new E.StructResolver(), __type_U);
            this.nlneu = new ObjectStructField<List<E<U>>>(
                    this,
                    BondType.nullableOf(
                            BondType.listOf(
                                    BondType.nullableOf((StructBondType<E<U>>) __spec_neu__1))),
                    5,
                    "nlneu",
                    Modifier.Optional,
                    false);

            // initialize struct descriptor
            super.initializeFields(
                    this.u,
                    this.neu,
                    this.lneu,
                    this.nlneu
            );
        }

        @Override
        public final Class<E<U>> getValueClass() {
            Class clazz = E.class;
            return (Class<E<U>>) clazz;
        }

        @Override
        public final E<U> newInstance() {
            return new E<U>(this);
        }

        @Override
        protected final void serializeStructFields(
                SerializationContext context, E<U> value) throws IOException {
            this.u.serializeObject(context, value.u);
            this.neu.serializeObject(context, value.neu);
            this.lneu.serializeObject(context, value.lneu);
            this.nlneu.serializeObject(context, value.nlneu);
        }

        @Override
        protected final void deserializeStructFields(
                TaggedDeserializationContext context, E<U> value) throws IOException {
            boolean __has_u = false;
            boolean __has_neu = false;
            boolean __has_lneu = false;
            boolean __has_nlneu = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.u = this.u.deserializeObject(context, __has_u);
                        __has_u = true;
                        break;
                    case 2:
                        value.neu = this.neu.deserializeObject(context, __has_neu);
                        __has_neu = true;
                        break;
                    case 3:
                        value.lneu = this.lneu.deserializeObject(context, __has_lneu);
                        __has_lneu = true;
                        break;
                    case 4:
                        value.nlneu = this.nlneu.deserializeObject(context, __has_nlneu);
                        __has_nlneu = true;
                        break;
                }
            }

            this.u.verifyDeserializedField(__has_u);
            this.neu.verifyDeserializedField(__has_neu);
            this.lneu.verifyDeserializedField(__has_lneu);
            this.nlneu.verifyDeserializedField(__has_nlneu);
        }

        private void initializeFieldValues(E<U> value) {
            value.u = this.u.initializeObject();
            value.neu = this.neu.initializeObject();
            value.lneu = this.lneu.initializeObject();
            value.nlneu = this.nlneu.initializeObject();
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
    private final Struct<U> __struct;

    // struct fields
    public U u;
    public E<U> neu;
    public List<E<U>> lneu;
    public List<E<U>> nlneu;

    // constructor that takes type specialization (cached)
    public E(Struct<U> struct) {
        this.__struct = struct;
        struct.initializeFieldValues(this);
    }

    // constructor that takes individual type parameters
    public E(BondType<U> U) {
        this(struct.resolve(U));
    }

    @Override
    public StructBondType<? extends BondSerializable> getStruct() {
        return this.__struct;
    }
}
