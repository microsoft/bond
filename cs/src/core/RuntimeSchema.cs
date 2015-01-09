// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System.Diagnostics;

    public struct RuntimeSchema
    {
        readonly SchemaDef schemaDef;
        readonly TypeDef typeDef;

        public static readonly RuntimeSchema Empty = new RuntimeSchema();
        public SchemaDef SchemaDef { get { return schemaDef; } }
        public TypeDef TypeDef { get { return typeDef; } }
        public StructDef StructDef { get { return schemaDef.structs[typeDef.struct_def]; } }
        public bool HasValue { get { return schemaDef != null && typeDef != null; } }
        public bool IsStruct { get { return HasValue && typeDef.id == BondDataType.BT_STRUCT; } }
        public bool IsBonded { get { return HasValue && typeDef.bonded_type; } }
        public bool IsBlob { get { return HasValue && typeDef.id == BondDataType.BT_LIST && typeDef.element.id == BondDataType.BT_INT8; } }
        public bool IsContainer { get { return HasValue && typeDef.element != null; } }
        public bool IsMap { get { return HasValue && typeDef.key != null; } }
        public bool HasBase { get { return IsStruct && StructDef.base_def != null; } }

        public RuntimeSchema(SchemaDef schema)
            : this(schema, schema.root)
        {}

        RuntimeSchema(SchemaDef schema, TypeDef type)
        {
            schemaDef = schema;
            typeDef = type;
        }

        public RuntimeSchema GetBaseSchema()
        {
            Debug.Assert(IsStruct);
            return new RuntimeSchema(schemaDef, StructDef.base_def);
        }

        public RuntimeSchema GetElementSchema()
        {
            Debug.Assert(HasValue);
            return new RuntimeSchema(schemaDef, typeDef.element);
        }

        public RuntimeSchema GetKeySchema()
        {
            Debug.Assert(HasValue);
            return new RuntimeSchema(schemaDef, typeDef.key);
        }

        public RuntimeSchema GetFieldSchema(FieldDef field)
        {
            Debug.Assert(HasValue);
            return new RuntimeSchema(schemaDef, field.type);
        }
    }
}
