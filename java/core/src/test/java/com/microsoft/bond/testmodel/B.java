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

    // implementation of the type descriptor resolver
    public static final class StructResolver extends StructBondTypeResolver<B> {

        // register a struct resolver instance so that it can be
        // retrieved by calling static methods of the BondType class
        static {
            registerStructType(B.class, new StructResolver());
        }

        // public type resolver method customized to the generic type parameters
        public final <T> Struct<T> resolve(BondType<T> T) {
            ArgumentHelper.ensureNotNull(T, "T");
            return (Struct<T>) (StructBondType) this.resolveAndInitialize(T);
        }

        @Override
        protected final Struct resolveUninitialized(BondType<?>... genericTypeArguments) {
            BondType<?> T = getCachedType(genericTypeArguments[0]);
            GenericTypeSpecialization specialization = new GenericTypeSpecialization(T);
            StructBondType<? super B> base = resolveUninitializedWithCaching(new A.StructResolver(), BondTypes.STRING, T);
            return (Struct) getCachedType(new Struct(base, specialization));
        }
    }

    // implementation of the type descriptor
    public static final class Struct<T> extends StructBondType<B<T>> {

        // field descriptors for each field in the struct
        private StructField<T> t;
        private StructField<A<T, T>> at;
        private StructField<B<T>> nbt;
        private StructField<E<T>> net;
        private StructField<List<T>> lt1;
        private StructField<List<List<T>>> lt2;
        private StructField<List<List<List<T>>>> lt3;
        private StructField<List<List<List<List<T>>>>> lt4;
        private StructField<List<List<List<List<List<T>>>>>> lt5;

        // restrict instantiation to the enclosing class and its members
        private Struct(StructBondType<? super B> base, GenericTypeSpecialization specialization) {
            super(Struct.class, base, specialization);
        }

        @Override
        protected final void initialize() {
            BondType<T> __type_T = this.getGenericSpecialization().getGenericTypeArgument(0);

            // initialize field descriptor
            this.t = new ObjectStructField<T>(
                    this,
                    __type_T,
                    1,
                    "t",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_at__1 =
                    resolveUninitializedWithCaching(new A.StructResolver(), __type_T, __type_T);
            this.at = new ObjectStructField<A<T, T>>(
                    this,
                    (StructBondType<A<T, T>>) __spec_at__1,
                    2,
                    "at",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_nbt__1 =
                    resolveUninitializedWithCaching(new B.StructResolver(), __type_T);
            this.nbt = new ObjectStructField<B<T>>(
                    this,
                    BondType.nullableOf((StructBondType<B<T>>) __spec_nbt__1),
                    3,
                    "nbt",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            StructBondType __spec_net__1 =
                    resolveUninitializedWithCaching(new E.StructResolver(), __type_T);
            this.net = new ObjectStructField<E<T>>(
                    this,
                    BondType.nullableOf((StructBondType<E<T>>) __spec_net__1),
                    4,
                    "net",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.lt1 = new ObjectStructField<List<T>>(
                    this,
                    BondType.listOf(__type_T),
                    5,
                    "lt1",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.lt2 = new ObjectStructField<List<List<T>>>(
                    this,
                    BondType.listOf(
                            BondType.listOf(__type_T)),
                    6,
                    "lt2",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.lt3 = new ObjectStructField<List<List<List<T>>>>(
                    this,
                    BondType.listOf(
                            BondType.listOf(
                                    BondType.listOf(__type_T))),
                    7,
                    "lt3",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.lt4 = new ObjectStructField<List<List<List<List<T>>>>>(
                    this,
                    BondType.listOf(
                            BondType.listOf(
                                    BondType.listOf(
                                            BondType.listOf(__type_T)))),
                    8,
                    "lt4",
                    Modifier.Optional,
                    false);

            // initialize field descriptor
            this.lt5 = new ObjectStructField<List<List<List<List<List<T>>>>>>(
                    this,
                    BondType.listOf(
                            BondType.listOf(
                                    BondType.listOf(
                                            BondType.listOf(
                                                    BondType.listOf(__type_T))))),
                    9,
                    "lt5",
                    Modifier.Optional,
                    false);

            // initialize struct descriptor
            super.initializeFields(
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
            Class clazz = B.class;
            return (Class<B<T>>) clazz;
        }

        @Override
        public final B<T> newInstance() {
            return new B<T>(this);
        }

        @Override
        protected final void serializeStructFields(
                SerializationContext context, B<T> value) throws IOException {
            this.t.serializeObject(context, value.t);
            this.at.serializeObject(context, value.at);
            this.nbt.serializeObject(context, value.nbt);
            this.net.serializeObject(context, value.net);
            this.lt1.serializeObject(context, value.lt1);
            this.lt2.serializeObject(context, value.lt2);
            this.lt3.serializeObject(context, value.lt3);
            this.lt4.serializeObject(context, value.lt4);
            this.lt5.serializeObject(context, value.lt5);
        }

        @Override
        protected final void deserializeStructFields(
                TaggedDeserializationContext context, B<T> value) throws IOException {
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
                        value.t = this.t.deserializeObject(context, __has_t);
                        __has_t = true;
                        break;
                    case 2:
                        value.at = this.at.deserializeObject(context, __has_at);
                        __has_at = true;
                        break;
                    case 3:
                        value.nbt = this.nbt.deserializeObject(context, __has_nbt);
                        __has_nbt = true;
                        break;
                    case 4:
                        value.net = this.net.deserializeObject(context, __has_net);
                        __has_net = true;
                        break;
                    case 5:
                        value.lt1 = this.lt1.deserializeObject(context, __has_lt1);
                        __has_lt1 = true;
                        break;
                    case 6:
                        value.lt2 = this.lt2.deserializeObject(context, __has_lt2);
                        __has_lt2 = true;
                        break;
                    case 7:
                        value.lt3 = this.lt3.deserializeObject(context, __has_lt3);
                        __has_lt3 = true;
                        break;
                    case 8:
                        value.lt4 = this.lt4.deserializeObject(context, __has_lt4);
                        __has_lt4 = true;
                        break;
                    case 9:
                        value.lt5 = this.lt5.deserializeObject(context, __has_lt5);
                        __has_lt5 = true;
                        break;
                }
            }

            this.t.verifyDeserializedField(__has_t);
            this.at.verifyDeserializedField(__has_at);
            this.nbt.verifyDeserializedField(__has_nbt);
            this.net.verifyDeserializedField(__has_net);
            this.lt1.verifyDeserializedField(__has_lt1);
            this.lt2.verifyDeserializedField(__has_lt2);
            this.lt3.verifyDeserializedField(__has_lt3);
            this.lt4.verifyDeserializedField(__has_lt4);
            this.lt5.verifyDeserializedField(__has_lt5);
        }

        private void initializeFieldValues(B<T> value) {
            value.t = this.t.initializeObject();
            value.at = this.at.initializeObject();
            value.nbt = this.nbt.initializeObject();
            value.net = this.net.initializeObject();
            value.lt1 = this.lt1.initializeObject();
            value.lt2 = this.lt2.initializeObject();
            value.lt3 = this.lt3.initializeObject();
            value.lt4 = this.lt4.initializeObject();
            value.lt5 = this.lt5.initializeObject();
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
    private final Struct<T> __struct;

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

    // constructor that takes type specialization (cached)
    public B(Struct<T> struct) {
        super(getBase(struct));
        this.__struct = struct;
        struct.initializeFieldValues(this);
    }

    // private helper to check constructor argument
    private static <T> A.Struct<String, T> getBase(Struct<T> struct) {
        ArgumentHelper.ensureNotNull(struct, "struct");
        return (A.Struct) struct.getBaseStructType();
    }

    // constructor that takes individual type parameters
    public B(BondType<T> T) {
        this(struct.resolve(T));
    }

    @Override
    public StructBondType<? extends BondSerializable> getStruct() {
        return this.__struct;
    }
}
