// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// A helper type that redirects methods from Bond.BondReflection to
    /// their original name in Bond.Reflection.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The type Bond.Reflection was accidentally renamed to
    /// Bond.BondReflection in C# Bond 5.1.0, breaking backward
    /// compatibility. This has been fixed. Until the next major release of
    /// C# Bond, this type exists to redirect anyone who started using
    /// Bond.BondReflection in the interim.
    /// </para>
    /// <para>
    /// This type will be removed during or after the next major release of
    /// C# Bond.
    /// </para>
    /// </remarks>
    [Obsolete("Use Bond.Reflection instead. Bond.BondReflection will be removed in a subsequent version.")]
    public static class BondReflection
    {
        /// <summary>
        /// Get list of fields for a Bond schema
        /// </summary>
        [Obsolete("Use Bond.Reflection.GetGetSchemaFields instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static IEnumerable<ISchemaField> GetSchemaFields(Type type)
        {
            return Reflection.GetSchemaFields(type);
        }

        /// <summary>
        /// Get the inner Type of composite/container types
        /// </summary>
        [Obsolete("Use Bond.Reflection.GetValueType instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static Type GetValueType(Type type)
        {
            return Reflection.GetValueType(type);
        }

        /// <summary>
        /// Get the key and value Type for a map
        /// </summary>
        [Obsolete("Use Bond.Reflection.GetKeyValueType instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static KeyValuePair<Type, Type> GetKeyValueType(Type type)
        {
            return Reflection.GetKeyValueType(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond schema
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondStruct instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondStruct(Type type)
        {
            return Reflection.IsBondStruct(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a bonded&lt;T>
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBonded instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBonded(Type type)
        {
            return Reflection.IsBonded(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond nullable type
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondNullable instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondNullable(Type type)
        {
            return Reflection.IsBondNullable(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond string
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondString instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondString(Type type)
        {
            return Reflection.IsBondString(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond blob
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondBlob instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondBlob(Type type)
        {
            return Reflection.IsBondBlob(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond list
        /// or a Bond vector
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondList instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondList(Type type)
        {
            return Reflection.IsBondList(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond map
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondMap instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondMap(Type type)
        {
            return Reflection.IsBondMap(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond set
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondSet instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondSet(Type type)
        {
            return Reflection.IsBondSet(type);
        }

        /// <summary>
        /// Get a value indicating whether the Type is a Bond container
        /// </summary>
        [Obsolete("Use Bond.Reflection.IsBondContainer instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static bool IsBondContainer(Type type)
        {
            return Reflection.IsBondContainer(type);
        }

        /// <summary>
        /// Get the BondDataType value for the Type
        /// </summary>
        [Obsolete("Use Bond.Reflection.GetBondDataType instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static BondDataType GetBondDataType(Type type)
        {
            return Reflection.GetBondDataType(type);
        }

        /// <summary>
        /// Get the ListSubType value for the Type
        /// </summary>
        [Obsolete("Use Bond.Reflection.GetBondListDataType instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static ListSubType GetBondListDataType(Type type)
        {
            return Reflection.GetBondListDataType(type);
        }

        /// <summary>
        /// Get the Type representing the base schema or null if the schema has no base
        /// </summary>
        [Obsolete("Use Bond.Reflection.GetBaseSchemaType instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static Type GetBaseSchemaType(Type type)
        {
            return Reflection.GetBaseSchemaType(type);
        }

        /// <summary>
        /// Get the Type of the schema field, including any type annotations from TypeAttribute
        /// </summary>
        /// <remarks>
        /// In some cases this may not be the actual type of the property or field.
        /// If the property or field has a TypeAttribute, this will be the attribute's value
        /// and can provide schema information that is not available on the actual 
        /// property/field type.
        /// </remarks>
        [Obsolete("Use Bond.Reflection.GetSchemaType instead. Bond.BondReflection will be removed in a subsequent version.")]
        public static Type GetSchemaType(ISchemaField schemaField)
        {
            return Reflection.GetSchemaType(schemaField);
        }
    }
}
