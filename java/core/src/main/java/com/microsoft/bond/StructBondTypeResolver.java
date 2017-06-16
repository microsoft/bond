// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

/**
 * Responsible for resolving generic Bond struct types.
 * @param <TStruct> the class of the struct value
 */
public abstract class StructBondTypeResolver<TStruct extends BondSerializable> {

    /**
     * Resolves generic type arguments and creates a new instance of type descriptor that is not yet initialized.
     * Generated sublasses add a public method to resolve and initialized the type descriptor.
     *
     * @param genericTypeArguments generic type arguments
     * @return new unitialized type descriptor instance
     */
    protected abstract StructBondType<TStruct> resolveUninitialized(BondType<?>[] genericTypeArguments);

    /**
     * Resolves generic type arguments and creates and initializes new instance of type descriptor.
     * Generated sublasses add a public method to resolve and initialized the type descriptor.
     *
     * @param genericTypeArguments generic type arguments
     * @return new unitialized type descriptor instance
     */
    protected final StructBondType<TStruct> resolveAndInitialize(BondType<?>... genericTypeArguments) {
        StructBondType<TStruct> type = this.resolveUninitialized(genericTypeArguments);
        ensureInitialized(type);
        return type;
    }

    /**
     * Resolves generic type arguments and caches the result in the type cache.
     *
     * @param resolver             type resolver
     * @param genericTypeArguments generic type arguments
     * @param <TStruct>            the class of the struct value
     * @return a cached uninitialized type descriptor
     */
    protected static <TStruct extends BondSerializable> StructBondType<TStruct> resolveUninitializedWithCaching(
            StructBondTypeResolver<TStruct> resolver, BondType<?>... genericTypeArguments) {
        return StructBondType.resolveUninitializedWithCaching(resolver, genericTypeArguments);
    }

    /**
     * Returns a cached type descriptor that is equal to the argument, or caches the argument otherwise.
     *
     * @param type the type descriptor
     * @param <T>  the Bond value class
     * @return a cached type descriptor
     */
    protected static <T> BondType<T> getCachedType(BondType<T> type) {
        return BondType.getCachedType(type);
    }

    /**
     * Registers a struct type resolver from generated code.
     *
     * @param clazz              the struct class
     * @param structTypeResolver type resolver for the given struct type
     * @param <TStruct>          the Bond struct value class
     */
    protected static <TStruct extends BondSerializable> void registerStructType(
            Class<TStruct> clazz, StructBondTypeResolver<TStruct> structTypeResolver) {
        BondType.registerStructType(clazz, structTypeResolver);
    }

    /**
     * Called from generated code to make sure the type descriptor is initialized
     * as well as all its referenced struct types.
     */
    protected static void ensureInitialized(StructBondType<?> type) {
        type.ensureInitialized();
    }
}
