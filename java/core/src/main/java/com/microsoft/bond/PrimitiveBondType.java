// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import java.util.HashMap;

/**
 * Partially implements the {@link BondType} contract for primitive data types.
 * @param <TPrimitive> the class of the primitive value
 */
public abstract class PrimitiveBondType<TPrimitive> extends BondType<TPrimitive> {

    // restrict subclasses to the current package
    PrimitiveBondType(Class<? extends PrimitiveBondType<TPrimitive>> thisClass) {
        super(thisClass.hashCode());
    }

    @Override
    public final boolean isNullableType() {
        return false;
    }

    @Override
    public final boolean isGenericType() {
        return false;
    }

    @Override
    public final BondType<?>[] getGenericTypeArguments() {
        return null;
    }

    @Override
    final boolean equalsInternal(BondType<?> obj) {
        // the caller makes sure that the class of the argument is the same as the class of this object,
        // and since primitive types are singletons all instance are considered equal
        return true;
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        TypeDef typeDef = new TypeDef();
        typeDef.id = this.getBondDataType();
        return typeDef;
    }
}
