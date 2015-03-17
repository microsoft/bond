-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Bond.Schema.JSON
    ( FromJSON(..)
    ) where

import Bond.Schema.Types
import Data.Aeson

instance FromJSON Modifier
instance ToJSON Modifier

instance FromJSON Type
instance ToJSON Type where
    toJSON BT_Int8 = "int8"
    toJSON BT_Int16 = "int16"
    toJSON BT_Int32 = "int32"
    toJSON BT_Int64 = "int64"
    toJSON BT_UInt8 = "uint8"
    toJSON BT_UInt16 = "uint16"
    toJSON BT_UInt32 = "uint32"
    toJSON BT_UInt64 = "uint64"
    toJSON BT_Float = "float"
    toJSON BT_Double = "double"
    toJSON BT_Bool = "bool"
    toJSON BT_String = "string"
    toJSON BT_WString = "wstring"
    toJSON BT_MetaName = "bond_meta::name"
    toJSON BT_MetaFullName = "bond_meta::full_name"
    toJSON BT_Blob = "blob"
    toJSON (BT_Maybe t) = object
        [ "type" .= String "maybe"
        , "element" .= t
        ]
    toJSON (BT_List t) = object
        [ "type" .= String "list"
        , "element" .= t
        ]
    toJSON (BT_Vector t) = object
        [ "type" .= String "vector"
        , "element" .= t
        ]
    toJSON (BT_Nullable t) = object
        [ "type" .= String "nullable"
        , "element" .= t
        ]
    toJSON (BT_Set t) = object
        [ "type" .= String "set"
        , "element" .= t
        ]
    toJSON (BT_Map k t) = object
        [ "type" .= String "map"
        , "key" .= k
        , "element" .= t
        ]
    toJSON (BT_Bonded t) = object
        [ "type" .= String "bonded"
        , "element" .= t
        ]
    toJSON (BT_IntTypeArg n) = object
        [ "type" .= String "constant"
        , "value" .= n
        ]
    toJSON (BT_TypeParam TypeParam {..}) = object
        [ "type" .= String "parameter"
        , "name" .= paramName
        , "constraint" .= paramConstraint
        ]
    toJSON (BT_UserDefined decl args) = object
        [ "type" .= String "user"
        , "declaration" .= decl
        , "arguments" .= args
        ]


instance FromJSON Default
instance ToJSON Default where
    toJSON DefaultNothing = object [ "value" .= Null ]
    toJSON (DefaultBool x) = toJSON x
    toJSON (DefaultInteger x) = toJSON x
    toJSON (DefaultFloat x) = toJSON x
    toJSON (DefaultString x) = toJSON x
    toJSON (DefaultEnum x) = toJSON x

instance FromJSON Attribute
instance ToJSON Attribute

instance FromJSON Field
instance ToJSON Field

instance FromJSON Constant
instance ToJSON Constant

instance FromJSON Constraint
instance ToJSON Constraint where
    toJSON Value = "value"

instance FromJSON TypeParam
instance ToJSON TypeParam

instance FromJSON Declaration
instance ToJSON Declaration

instance FromJSON Import
instance ToJSON Import

instance FromJSON Language
instance ToJSON Language

instance FromJSON Namespace
instance ToJSON Namespace

instance FromJSON Bond
instance ToJSON Bond

