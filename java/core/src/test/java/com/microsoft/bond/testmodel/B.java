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
 struct B<T> : A<string, T>
 {
     1 : T t;
     2 : A<T, T> at;
     3 : nullable<B<T>> nbt;
     4 : nullable<E<T>> net;
     5 : list<T> lt1;
     6 : list<list<T>> lt2;
     7 : list<list<list<T>>> lt3;
     8 : list<list<list<list<T>>>> lt4;
     9 : list<list<list<list<list<T>>>>> lt5;
 }
 </pre>
 *
 */
@SuppressWarnings("unchecked")
public class B<T> extends A<String, T> implements BondSerializable {

    // public definition of the type descriptor builder for generic type
    public static abstract class GenericBondTypeBuilder {

        // hide constructor to prevent subclassing outside of the current scope
        private GenericBondTypeBuilder() {
        }

        // public API to make an instance of a generic type
        public abstract <T> StructBondType<B<T>> makeGenericType(BondType<T> T);
    }

    // private implementation of the type descriptor
    private static final class StructBondTypeImpl<T> extends StructBondType<B<T>> {

        // private implementation of the type descriptor builder
        static final class StructBondTypeBuilderImpl extends StructBondTypeBuilder<B> {

            // called by the public method to make an instance of a generic type
            private <T> StructBondType<B<T>> makeGenericType(BondType<T> T) {
                ArgumentHelper.ensureNotNull(T, "T");
                StructBondType<?> structBondType = this.getInitializedFromCache(T);
                return (StructBondTypeImpl<T>) structBondType;
            }

            @Override
            public final int getGenericTypeParameterCount() {
                return 1;
            }

            @Override
            protected final StructBondType<B> buildNewInstance(BondType<?>[] genericTypeArguments) {
                return new StructBondTypeImpl(new GenericTypeSpecialization(genericTypeArguments));
            }

            // registration method
            private static void register() {
                registerStructType(B.class, new StructBondTypeBuilderImpl());
            }
        }

        // field descriptors for each field in the struct
        private ObjectStructField<T> t;
        private ObjectStructField<A<T, T>> at;
        private ObjectStructField<B<T>> nbt;
        private ObjectStructField<E<T>> net;
        private ObjectStructField<List<T>> lt1;
        private ObjectStructField<List<List<T>>> lt2;
        private ObjectStructField<List<List<List<T>>>> lt3;
        private ObjectStructField<List<List<List<List<T>>>>> lt4;
        private ObjectStructField<List<List<List<List<List<T>>>>>> lt5;

        // restrict instantiation to the enclosing class and its members
        private StructBondTypeImpl(GenericTypeSpecialization genericTypeSpecialization) {
            super(genericTypeSpecialization);
        }

        @Override
        protected final void initialize() {
            BondType<T> T = this.getGenericSpecialization().getGenericTypeArgument(0);

            // initialize field descriptor
            this.t = new ObjectStructField<T>(
                    this,
                    T,
                    1,
                    "t",
                    Modifier.Optional);

            // initialize field descriptor
            this.at = new ObjectStructField<A<T, T>>(
                    this,
                    (StructBondType<A<T, T>>) getStructType(A.class, T, T),
                    2,
                    "at",
                    Modifier.Optional);

            // initialize field descriptor
            this.nbt = new ObjectStructField<B<T>>(
                    this,
                    nullableOf((StructBondType<B<T>>) getStructType(B.class, T)),
                    3,
                    "nbt",
                    Modifier.Optional);

            // initialize field descriptor
            this.net = new ObjectStructField<E<T>>(
                    this,
                    nullableOf((StructBondType<E<T>>) getStructType(E.class, T)),
                    4,
                    "net",
                    Modifier.Optional);

            // initialize field descriptor
            this.lt1 = new ObjectStructField<List<T>>(
                    this,
                    listOf(T),
                    5,
                    "lt1",
                    Modifier.Optional);

            // initialize field descriptor
            this.lt2 = new ObjectStructField<List<List<T>>>(
                    this,
                    listOf(listOf(T)),
                    6,
                    "lt2",
                    Modifier.Optional);

            // initialize field descriptor
            this.lt3 = new ObjectStructField<List<List<List<T>>>>(
                    this,
                    listOf(listOf(listOf(T))),
                    7,
                    "lt3",
                    Modifier.Optional);

            // initialize field descriptor
            this.lt4 = new ObjectStructField<List<List<List<List<T>>>>>(
                    this,
                    listOf(listOf(listOf(listOf(T)))),
                    8,
                    "lt4",
                    Modifier.Optional);

            // initialize field descriptor
            this.lt5 = new ObjectStructField<List<List<List<List<List<T>>>>>>(
                    this,
                    listOf(listOf(listOf(listOf(listOf(T))))),
                    9,
                    "lt5",
                    Modifier.Optional);

            // initialize struct descriptor
            super.initializeBaseAndFields(
                    (StructBondType<A<String, T>>) getStructType(A.class, BondTypes.STRING, T),
                    this.t,
                    this.at,
                    this.nbt,
                    this.net,
                    this.lt1,
                    this.lt2,
                    this.lt3,
                    this.lt4,
                    this.lt5
            );
        }

        @Override
        public final Class<B<T>> getValueClass() {
            return (Class<B<T>>) (Class<?>) B.class;
        }

        @Override
        public final B<T> newInstance() {
            return new B<T>(this);
        }

        @Override
        protected final void serializeStructFields(SerializationContext context, B<T> value) throws IOException {
            this.t.serialize(context, value.t);
            this.at.serialize(context, value.at);
            this.nbt.serialize(context, value.nbt);
            this.net.serialize(context, value.net);
            this.lt1.serialize(context, value.lt1);
            this.lt2.serialize(context, value.lt2);
            this.lt3.serialize(context, value.lt3);
            this.lt4.serialize(context, value.lt4);
            this.lt5.serialize(context, value.lt5);
        }

        @Override
        protected final void deserializeStructFields(TaggedDeserializationContext context, B<T> value) throws IOException {
            boolean __has_t = false;
            boolean __has_at = false;
            boolean __has_nbt = false;
            boolean __has_net = false;
            boolean __has_lt1 = false;
            boolean __has_lt2 = false;
            boolean __has_lt3 = false;
            boolean __has_lt4 = false;
            boolean __has_lt5 = false;
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    case 1:
                        value.t = this.t.deserialize(context, __has_t);
                        __has_t = true;
                        break;
                    case 2:
                        value.at = this.at.deserialize(context, __has_at);
                        __has_at = true;
                        break;
                    case 3:
                        value.nbt = this.nbt.deserialize(context, __has_nbt);
                        __has_nbt = true;
                        break;
                    case 4:
                        value.net = this.net.deserialize(context, __has_net);
                        __has_net = true;
                        break;
                    case 5:
                        value.lt1 = this.lt1.deserialize(context, __has_lt1);
                        __has_lt1 = true;
                        break;
                    case 6:
                        value.lt2 = this.lt2.deserialize(context, __has_lt2);
                        __has_lt2 = true;
                        break;
                    case 7:
                        value.lt3 = this.lt3.deserialize(context, __has_lt3);
                        __has_lt3 = true;
                        break;
                    case 8:
                        value.lt4 = this.lt4.deserialize(context, __has_lt4);
                        __has_lt4 = true;
                        break;
                    case 9:
                        value.lt5 = this.lt5.deserialize(context, __has_lt5);
                        __has_lt5 = true;
                        break;
                    default:
                        context.reader.skip(context.readFieldResult.type);
                        break;
                }
            }

            this.t.verifyDeserialized(__has_t);
            this.at.verifyDeserialized(__has_at);
            this.nbt.verifyDeserialized(__has_nbt);
            this.net.verifyDeserialized(__has_net);
            this.lt1.verifyDeserialized(__has_lt1);
            this.lt2.verifyDeserialized(__has_lt2);
            this.lt3.verifyDeserialized(__has_lt3);
            this.lt4.verifyDeserialized(__has_lt4);
            this.lt5.verifyDeserialized(__has_lt5);
        }

        @Override
        public final void initializeStructFields(B<T> value) {
            value.t = this.t.initialize();
            value.at = this.at.initialize();
            value.nbt = this.nbt.initialize();
            value.net = this.net.initialize();
            value.lt1 = this.lt1.initialize();
            value.lt2 = this.lt2.initialize();
            value.lt3 = this.lt3.initialize();
            value.lt4 = this.lt4.initialize();
            value.lt5 = this.lt5.initialize();
        }

        @Override
        protected void cloneStructFields(B<T> fromValue, B<T> toValue) {
            toValue.t = this.t.clone(fromValue.t);
            toValue.at = this.at.clone(fromValue.at);
            toValue.nbt = this.nbt.clone(fromValue.nbt);
            toValue.net = this.net.clone(fromValue.net);
            toValue.lt1 = this.lt1.clone(fromValue.lt1);
            toValue.lt2 = this.lt2.clone(fromValue.lt2);
            toValue.lt3 = this.lt3.clone(fromValue.lt3);
            toValue.lt4 = this.lt4.clone(fromValue.lt4);
            toValue.lt5 = this.lt5.clone(fromValue.lt5);
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
        public final <T> StructBondType<B<T>> makeGenericType(BondType<T> T) {
            return this.builder.makeGenericType(T);
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
    private final StructBondTypeImpl<T> __genericType;

    // struct fields
    public T t;
    public A<T, T> at;
    public B<T> nbt;
    public E<T> net;
    public List<T> lt1;
    public List<List<T>> lt2;
    public List<List<List<T>>> lt3;
    public List<List<List<List<T>>>> lt4;
    public List<List<List<List<List<T>>>>> lt5;

    // the only constructor which takes the generic type specialization
    public B(StructBondType<B<T>> genericType) {
        super((StructBondType<A<String, T>>) ArgumentHelper.ensureNotNull(genericType, "genericType").getBaseStructType());
        this.__genericType = (StructBondTypeImpl<T>) genericType;
        this.__genericType.initializeStructFields(this);
    }

    @Override
    public StructBondType<? extends B<T>> getBondType() {
        return this.__genericType;
    }
}
