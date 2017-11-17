// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/nonassignable.h"

#include <bond/core/bond_types.h>

#include <boost/shared_ptr.hpp>

namespace bond
{


struct SchemaDef;
struct TypeDef;
struct StructDef;
struct FieldDef;

/// @brief Represents runtime schema
/// See [User's Manual](../../manual/bond_cpp.html#runtime-schema)
class RuntimeSchema
{
public:
    /// @brief Default constructor
    RuntimeSchema()
        : schema(NULL),
          type(NULL)
    {}

    RuntimeSchema(RuntimeSchema&& rhs) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<boost::shared_ptr<SchemaDef> >::value)
        : schema(rhs.schema),
          type(rhs.type),
          instance(std::move(rhs.instance))
    {}

    RuntimeSchema& operator=(const RuntimeSchema& that) = default;

    /// @brief Construct from a share_ptr to a SchemaDef object
    RuntimeSchema(const boost::shared_ptr<SchemaDef>& schema);

    /// @brief Construct from a reference to a SchemaDef object
    ///
    /// This ctor should be used only when it can be guaranteed that liftime of
    /// the SchemaDef object referenced by the schema argument is longer than
    /// lifetime of any copy of the RuntimeSchema object being constructed.
    /// In most cases it is safer to use the ctor taking shared_ptr<SchemaDef>.
    explicit RuntimeSchema(const SchemaDef& schema);

    RuntimeSchema(const RuntimeSchema& schema);
    RuntimeSchema(const RuntimeSchema& schema, const TypeDef& type);
    RuntimeSchema(const RuntimeSchema& schema, const FieldDef& field);

    const RuntimeSchema* get() const
    {
        return schema ? this : NULL;
    }

    /// @brief Returns constant reference to SchemaDef object
    const SchemaDef& GetSchema() const
    {
        return *schema;
    }

    const TypeDef& GetType() const
    {
        return *type;
    }

    bool HasBase() const;
    RuntimeSchema GetBaseSchema() const;
    const StructDef& GetStruct() const;
    BondDataType GetTypeId() const;

private:
    const SchemaDef* schema;
    const TypeDef* type;
    boost::shared_ptr<SchemaDef> instance;
};

} // namespace bond
