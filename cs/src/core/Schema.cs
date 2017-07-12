// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using System.Reflection;
    using Bond.Internal.Reflection;

    /// <summary>
    /// Utility to create runtime schema for dynamically specified Bond schema
    /// </summary>
    public static class Schema
    {
        /// <summary>
        /// Get runtime schema for the specified Bond schema
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <returns>Instance of <see cref="RuntimeSchema"/></returns>
        public static RuntimeSchema GetRuntimeSchema(Type type)
        {
            var runtimeSchema = typeof(Schema<>)
                .MakeGenericType(type)
                .GetDeclaredProperty("RuntimeSchema", typeof(RuntimeSchema));
            return (RuntimeSchema)runtimeSchema.GetValue(null);
        }
    }

    /// <summary>
    /// Utility to create runtime schema for statically specified Bond schema 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public static class Schema<T>
    {
        static readonly Cache instance = new Cache(typeof (T));

        /// <summary>
        /// Runtime schema for the Bond schema type T
        /// </summary>
        public static RuntimeSchema RuntimeSchema { get { return new RuntimeSchema(instance.Schema); } }

        internal static Metadata Metadata 
        {
            get
            {
                return instance != null ? instance.Metadata : Cache.GetMetadata(typeof(T));
            } 
        }

        internal static Metadata[] Fields
        {
            get
            {
                return instance != null ? instance.Fields : Cache.GetFields(typeof(T));
            }
        }

        class Cache
        {
            public readonly Metadata Metadata;
            public readonly Metadata[] Fields;
            public readonly SchemaDef Schema;
            
            public Cache(Type type)
            {
                Metadata = GetMetadata(type);
                Fields = GetFields(type);

                // ReSharper disable once UseObjectOrCollectionInitializer
                // The schema field must be instantiated before GetStructDef is called
                Schema = new SchemaDef();
                Schema.root.struct_def = GetStructDef(type, Metadata, Fields);
            }

            ushort GetStructDef(Type type, Metadata metadata, Metadata[] fields)
            {
                var index = Schema.structs.Count;
                var structDef = new StructDef();
                Schema.structs.Add(structDef);
                structDef.metadata = metadata;

                var baseType = type.GetBaseSchemaType();
                if (baseType != null)
                    structDef.base_def = GetTypeDef(baseType);

                var i = 0;
                foreach (var field in type.GetSchemaFields())
                {
                    var fieldDef = new FieldDef
                    {
                        id = field.Id,
                        metadata = fields[i++],
                        type = GetTypeDef(field.GetSchemaType())
                    };

                    structDef.fields.Add(fieldDef);
                }

                return (ushort) index;
            }

            TypeDef GetTypeDef(Type type)
            {
                TypeDef typeDef;

                if (type.IsBonded())
                {
                    typeDef = GetTypeDef(type.GetValueType());
                    typeDef.bonded_type = true;
                }
                else
                {
                    typeDef = new TypeDef {id = type.GetBondDataType()};
                }

                if (type.IsBondContainer() || type.IsBondNullable() || type.IsBondBlob())
                {
                    if (type.IsBondMap())
                    {
                        var itemType = type.GetKeyValueType();
                        typeDef.key = GetTypeDef(itemType.Key);
                        typeDef.element = GetTypeDef(itemType.Value);
                    }
                    else
                    {
                        typeDef.element = GetTypeDef(type.GetValueType());
                    }
                }

                if (type.IsBondStruct())
                {
                    var i = Schema.structs.FindIndex(
                        s => s.metadata.qualified_name.Equals(type.GetSchemaFullName()));
                    if (i != -1)
                    {
                        typeDef.struct_def = (ushort) i;
                    }
                    else
                    {
                        var schemaT = typeof (Schema<>).MakeGenericType(type);
                        var metadataProp = schemaT.GetTypeInfo().GetDeclaredProperty("Metadata");
                        var fieldsProp = schemaT.GetTypeInfo().GetDeclaredProperty("Fields");
                        typeDef.struct_def = GetStructDef(
                            type,
                            metadataProp.GetValue(null) as Metadata,
                            fieldsProp.GetValue(null) as Metadata[]);
                    }
                }

                return typeDef;
            }

            public static Metadata GetMetadata(Type type)
            {
                return new Metadata
                    {
                        name = type.GetSchemaName(),
                        qualified_name = type.GetSchemaFullName(),
                        attributes = GetAttributes(type.GetTypeInfo())
                    };
            }

            public static Metadata[] GetFields(Type type)
            {
                return (from field in type.GetSchemaFields() select new Metadata
                    {
                        name = field.Name,
                        attributes = GetAttributes(field.MemberInfo),
                        modifier = field.GetModifier(),
                        default_value = GetDefaultValue(field)
                    }).ToArray();
            }

            static Variant GetDefaultValue(ISchemaField schemaField)
            {
                var defaultValue = schemaField.GetDefaultValue();
                var variant = new Variant();

                if (defaultValue == null)
                {
                    if (!schemaField.GetSchemaType().IsBondNullable())
                        variant.nothing = true;
                }
                else
                {
                    Type defaultValueType = defaultValue.GetType();
                    Type schemaFieldType = schemaField.GetSchemaType();

                    if (schemaFieldType == typeof (Tag.wstring))
                        schemaFieldType = typeof (string);

                    bool alias = defaultValueType != schemaFieldType;

                    switch (schemaField.GetSchemaType().GetBondDataType())
                    {
                        case BondDataType.BT_BOOL:
                            variant.uint_value = alias ? 0ul : ((bool) defaultValue ? 1ul : 0ul);
                            break;

                        case BondDataType.BT_UINT8:
                        case BondDataType.BT_UINT16:
                        case BondDataType.BT_UINT32:
                        case BondDataType.BT_UINT64:
                            variant.uint_value = alias ? 0 : Convert.ToUInt64(defaultValue);
                            break;

                        case BondDataType.BT_INT8:
                        case BondDataType.BT_INT16:
                        case BondDataType.BT_INT32:
                        case BondDataType.BT_INT64:
                            variant.int_value = alias ? 0 : Convert.ToInt64(defaultValue);
                            break;

                        case BondDataType.BT_FLOAT:
                            variant.double_value = alias ? 0 : Convert.ToSingle(defaultValue);
                            break;

                        case BondDataType.BT_DOUBLE:
                            variant.double_value = alias ? 0 : Convert.ToDouble(defaultValue);
                            break;

                        case BondDataType.BT_STRING:
                            variant.string_value = alias ? string.Empty : (string)defaultValue;
                            break;

                        case BondDataType.BT_WSTRING:
                            variant.wstring_value = alias ? string.Empty : (string)defaultValue;
                            break;
                    }
                }

                return variant;
            }

            static Dictionary<string, string> GetAttributes(MemberInfo memberInfo)
            {
                var attributes = new Dictionary<string, string>(StringComparer.Ordinal);

                foreach (var a in memberInfo.GetCustomAttributes(typeof(AttributeAttribute), false))
                {
                    Debug.Assert(a is AttributeAttribute);
                    var aa = a as AttributeAttribute;
                    attributes.Add(aa.Name, aa.Value);
                }

                return attributes;
            }
        }
    }
}
