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

    // public definition of the type descriptor builder for generic type
    public static abstract class GenericBondTypeBuilder {

        // hide constructor to prevent subclassing outside of the current scope
        private GenericBondTypeBuilder() {
        }

        // public API to make an instance of a generic type
        public abstract <U> StructBondType<E<U>> makeGenericType(BondType<U> U);
    }

    // private implementation of the type descriptor
    private static final class StructBondTypeImpl<U> extends StructBondType<E<U>> {

        // private implementation of the type descriptor builder
        static final class StructBondTypeBuilderImpl extends StructBondTypeBuilder<E> {

            // called by the public method to make an instance of a generic type
            private <U> StructBondType<E<U>> makeGenericType(BondType<U> U) {
                ArgumentHelper.ensureNotNull(U, "U");
                StructBondType<?> structBondType = this.getInitializedFromCache(U);
                return (StructBondTypeImpl<U>) structBondType;
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 1;
            }

            @Override
            protected final StructBondType<E> buildNewInstance(BondType<?>[] genericTypeArguments) {
                return new StructBondTypeImpl(new GenericTypeSpecialization(genericTypeArguments));
            }

            // registration method
            private static void register() {
                registerStructType(E.class, new StructBondTypeBuilderImpl());
            }
        }

        // field descriptors for each field in the struct
        private ObjectStructField<U> u;
        private ObjectStructField<E<U>> neu;
        private ObjectStructField<List<E<U>>> lneu;
        private ObjectStructField<List<E<U>>> nlneu;

        StructBondTypeImpl(GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }

        @Override
        protected final void initialize() {
            BondType<U> U = this.getGenericSpecialization().getGenericTypeArgument(0);

            // initialize field descriptor
            this.u = new ObjectStructField<U>(
                    this,
                    U,
                    1,
                    "u",
                    Modifier.Optional);

            // initialize field descriptor
            this.neu = new ObjectStructField<E<U>>(
                    this,
                    nullableOf((StructBondType<E<U>>) getStructType(E.class, U)),
                    2,
                    "neu",
                    Modifier.Optional);

            // initialize field descriptor
            this.lneu = new ObjectStructField<List<E<U>>>(
                    this,
                    listOf(nullableOf((StructBondType<E<U>>) getStructType(E.class, U))),
                    3,
                    "lneu",
                    Modifier.Optional);

            // initialize field descriptor
            this.nlneu = new ObjectStructField<List<E<U>>>(
                    this,
                    nullableOf(listOf(nullableOf((StructBondType<E<U>>) getStructType(E.class, U)))),
                    5,
                    "nlneu",
                    Modifier.Optional);

            // initialize struct descriptor
            super.initializeBaseAndFields(
                    (StructBondType<D>) getStructType(D.class),
                    this.u,
                    this.neu,
                    this.lneu,
                    this.nlneu
            );
        }

        @Override
        public final Class<E<U>> getValueClass() {
            return (Class<E<U>>) (Class<?>) E.class;
        }

        @Override
        public final E<U> newInstance() {
            return new E<U>(this);
        }

        @Override
        protected final void serializeStructFields(SerializationContext context, E<U> value) throws IOException {
            this.u.serialize(context, value.u);
            this.neu.serialize(context, value.neu);
            this.lneu.serialize(context, value.lneu);
            this.nlneu.serialize(context, value.nlneu);
        }

        @Override
        protected final void deserializeStructFields(TaggedDeserializationContext context, E<U> value) throws IOException {
            boolean __has_u = false;
            boolean __has_neu = false;
            boolean __has_lneu = false;
            boolean __has_nlneu = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.u = this.u.deserialize(context, __has_u);
                        __has_u = true;
                        break;
                    case 2:
                        value.neu = this.neu.deserialize(context, __has_neu);
                        __has_neu = true;
                        break;
                    case 3:
                        value.lneu = this.lneu.deserialize(context, __has_lneu);
                        __has_lneu = true;
                        break;
                    case 4:
                        value.nlneu = this.nlneu.deserialize(context, __has_nlneu);
                        __has_nlneu = true;
                        break;
                }
            }

            this.u.verifyDeserialized(__has_u);
            this.neu.verifyDeserialized(__has_neu);
            this.lneu.verifyDeserialized(__has_lneu);
            this.nlneu.verifyDeserialized(__has_nlneu);
        }

        @Override
        public final void initializeStructFields(E<U> value) {
            value.u = this.u.initialize();
            value.neu = this.neu.initialize();
            value.lneu = this.lneu.initialize();
            value.nlneu = this.nlneu.initialize();
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
        public final <U> StructBondType<E<U>> makeGenericType(BondType<U> U) {
            return this.builder.makeGenericType(U);
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
    private final StructBondTypeImpl<U> __genericType;

    // struct fields
    public U u;
    public E<U> neu;
    public List<E<U>> lneu;
    public List<E<U>> nlneu;

    // the only constructor which takes the generic type specialization
    public E(StructBondType<E<U>> genericType) {
        super();
        ArgumentHelper.ensureNotNull(genericType, "genericType");
        this.__genericType = (StructBondTypeImpl<U>) genericType;
        this.__genericType.initializeStructFields(this);
    }

    @Override
    public StructBondType<? extends BondSerializable> getBondType() {
        return this.__genericType;
    }
}
