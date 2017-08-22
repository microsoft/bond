// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.util.HashMap;

/**
 * Partially implements the {@link BondType} contract for all primitive Bond data types including enums.
 * All subclasses should implement the Singleton pattern and thus this base class implements non-overridable
 * {@link #hashCode()} and {@link #equals(Object)} methods that delegate to the identity of the implementation
 * class.
 * @param <TPrimitive> the class of the primitive value
 */
public abstract class PrimitiveBondType<TPrimitive> extends BondType<TPrimitive> {

    /**
     * Package-private constructor (extending BondType hierarchy by user code is not supported).
     */
    PrimitiveBondType() {
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
    protected final TPrimitive cloneValue(TPrimitive value) {
        return value;
    }

    @Override
    public final int hashCode() {
        // since the class is a singleton, delegate to the identity of the implementation class
        return this.getClass().hashCode();
    }

    @Override
    public final boolean equals(Object obj) {
        // since the class is a singleton, delegate to the identity of the implementation class
        return obj != null && this.getClass() == obj.getClass();
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        // initialize only with non-default values
        TypeDef typeDef = new TypeDef();
        typeDef.id = this.getBondDataType();
        return typeDef;
    }
}
