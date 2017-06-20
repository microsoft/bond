// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

/**
 * Declares constant Bond type descriptors.
 */
public final class BondTypes {

    // prevent instantiation
    private BondTypes() {
    }

    /**
     * A singleton type descriptor for the Bond "uint8" data type.
     */
    public static final UInt8BondType UINT8 = UInt8BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "uint16" data type.
     */
    public static final UInt16BondType UINT16 = UInt16BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "uint32" data type.
     */
    public static final UInt32BondType UINT32 = UInt32BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "uint64" data type.
     */
    public static final UInt64BondType UINT64 = UInt64BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "int8" data type.
     */
    public static final Int8BondType INT8 = Int8BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "int16" data type.
     */
    public static final Int16BondType INT16 = Int16BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "int32" data type.
     */
    public static final Int32BondType INT32 = Int32BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "int64" data type.
     */
    public static final Int64BondType INT64 = Int64BondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "bool" data type.
     */
    public static final BoolBondType BOOL = BoolBondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "float" data type.
     */
    public static final FloatBondType FLOAT = FloatBondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "double" data type.
     */
    public static final DoubleBondType DOUBLE = DoubleBondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "string" data type.
     */
    public static final StringBondType STRING = StringBondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "wstring" data type.
     */
    public static final WStringBondType WSTRING = WStringBondType.INSTANCE;

    /**
     * A singleton type descriptor for the Bond "blob" data type.
     */
    public static final BlobBondType BLOB = BlobBondType.INSTANCE;

    static {
        // cache primitive types for consistency
        BondType.typeCache.get(UINT8);
        BondType.typeCache.get(UINT16);
        BondType.typeCache.get(UINT32);
        BondType.typeCache.get(UINT64);
        BondType.typeCache.get(INT8);
        BondType.typeCache.get(INT16);
        BondType.typeCache.get(INT32);
        BondType.typeCache.get(INT64);
        BondType.typeCache.get(BOOL);
        BondType.typeCache.get(FLOAT);
        BondType.typeCache.get(DOUBLE);
        BondType.typeCache.get(STRING);
        BondType.typeCache.get(WSTRING);
        BondType.typeCache.get(BLOB);
    }
}
