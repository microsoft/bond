-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : provisional
Portability : portable

A suite of types describing the abstract syntax tree of the Bond
<https://microsoft.github.io/bond/manual/compiler.html#idl-syntax schema definition language>.
-}

module Language.Bond.Syntax.Types
    ( -- * Schema definition file
      Bond(..)
    , QualifiedName
    , Import(..)
    , Namespace(..)
      -- ** Declarations
    , Declaration(..)
    , Field(..)
    , Default(..)
    , Modifier(..)
    , Constant(..)
      -- ** Types
    , Type(..)
    , TypeParam(..)
    , Constraint(..)
      -- ** Services
    , MethodType(..)
    , methodTypeToMaybe
    , Method(..)
      -- ** Metadata
    , Attribute(..)
      -- ** Deprecated
    , Language(..)
    ) where

import Data.Word

-- | Represents fully qualified name
type QualifiedName = [String]

-- | Specifies whether a field is required or optional.
data Modifier =
    Optional |                              -- ^ field is optional and may be omitted during serialization
    Required |                              -- ^ field is required, deserialization will fail if it is missing
    RequiredOptional                        -- ^ deserialization will not fail if the field is missing but it can't be omitted during serialization
    deriving (Eq, Show)

-- | Type in the Bond type system
data Type =
    BT_Int8 | BT_Int16 | BT_Int32 | BT_Int64 |
    BT_UInt8 | BT_UInt16 | BT_UInt32 | BT_UInt64 |
    BT_Float | BT_Double |
    BT_Bool |
    BT_String | BT_WString |
    BT_MetaName | BT_MetaFullName |
    BT_Blob |
    BT_Maybe Type |                         -- ^ type for fields with the default value of @nothing@
    BT_List Type |
    BT_Vector Type |
    BT_Nullable Type |
    BT_Set Type |
    BT_Map Type Type |
    BT_Bonded Type |
    BT_IntTypeArg Int |                     -- ^ an integer argument in an instance of a generic type 'Alias'
    BT_TypeParam TypeParam |                -- ^ type parameter of a generic 'Struct' or 'Alias' declaration
    BT_UserDefined Declaration [Type]       -- ^ user defined type or an instance of a generic type with the specified type arguments
    deriving (Eq, Show)

-- | Default value of a field.
data Default =
    DefaultBool Bool |
    DefaultInteger Integer |
    DefaultFloat Double |
    DefaultString String |
    DefaultEnum String |                    -- ^ name of an enum 'Constant'
    DefaultNothing                          -- ^ explicitly specified default value of @nothing@
    deriving (Eq, Show)

-- | <https://microsoft.github.io/bond/manual/compiler.html#custom-attributes Attribute> for attaching user defined metadata to a 'Declaration' or a 'Field'
data Attribute =
    Attribute
        { attrName :: QualifiedName         -- attribute name
        , attrValue :: String               -- value
        }
    deriving (Eq, Show)

-- | Definition of a 'Struct' field.
data Field =
    Field
        { fieldAttributes :: [Attribute]    -- zero or more attributes
        , fieldOrdinal :: Word16            -- ordinal
        , fieldModifier :: Modifier         -- field modifier
        , fieldType :: Type                 -- type
        , fieldName :: String               -- field name
        , fieldDefault :: Maybe Default     -- optional default value
        }
    deriving (Eq, Show)

-- | Definition of an 'Enum' constant.
data Constant =
    Constant
        { constantName :: String            -- enum constant name
        , constantValue :: Maybe Int        -- optional constant value
        }
    deriving (Eq, Show)

-- | Constraint on a 'TypeParam'.
data Constraint = Value                     -- ^ the type parameter allows only value types
    deriving (Eq, Show)

-- | Type parameter of a <https://microsoft.github.io/bond/manual/compiler.html#generics generic> 'Struct' or type 'Alias'
data TypeParam =
    TypeParam
        { paramName :: String
        , paramConstraint :: Maybe Constraint
        }
    deriving (Eq, Show)

data MethodType = Void | Unary Type | Streaming Type
  deriving (Eq, Show)

-- | Method of a service
data Method =
    Function
        { methodAttributes :: [Attribute]   -- zero or more attributes
        , methodResult :: MethodType        -- method result
        , methodName :: String              -- method name
        , methodInput :: MethodType         -- method parameter
        }
    |
    Event
        { methodAttributes :: [Attribute]   -- zero or more attributes
        , methodName :: String              -- method name
        , methodInput :: MethodType         -- method parameter
        }
    deriving (Eq, Show)

-- | Converts a MethodType into a Maybe Type to ease the transition from the
-- current definition of Method (which uses MethodType for input/results)
-- and the previous definition which used Maybe Type.
--
-- This is intended to be used by codegen that doesn't yet support streaming
-- (e.g., C++ and Comm). Once that code has been updated to understand
-- streaming, this function will be removed.
--
-- Raises an error if given a Streaming type.
methodTypeToMaybe :: MethodType -> Maybe Type
methodTypeToMaybe Void = Nothing
methodTypeToMaybe (Unary t) = Just t
methodTypeToMaybe (Streaming t) = error ("Unable to handle streaming " ++ (show t) ++ " in this codegen mode.")

-- | Bond schema declaration
data Declaration =
    Struct
        { declNamespaces :: [Namespace]     -- namespace(s) in which the struct is declared
        , declAttributes :: [Attribute]     -- zero or more attributes
        , declName :: String                -- struct identifier
        , declParams :: [TypeParam]         -- list of type parameters for generics
        , structBase :: Maybe Type          -- optional base struct
        , structFields :: [Field]           -- zero or more fields
        }
    |                                       -- ^ <https://microsoft.github.io/bond/manual/compiler.html#struct-definition struct definition>
    Enum
        { declNamespaces :: [Namespace]     -- namespace(s) in which the enum is declared
        , declAttributes :: [Attribute]     -- zero or more attributes
        , declName :: String                -- enum identifier
        , enumConstants :: [Constant]       -- one or more enum constant values
        }
    |                                       -- ^ <https://microsoft.github.io/bond/manual/compiler.html#enum-definition enum definition>
    Forward
        { declNamespaces :: [Namespace]     -- namespace(s) in which the struct is declared
        , declName :: String                -- struct identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        }
    |                                       -- ^ <https://microsoft.github.io/bond/manual/compiler.html#forward-declaration forward declaration>
    Alias
        { declNamespaces :: [Namespace]     -- namespace(s) in which the type alias is declared
        , declName :: String                -- alias identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        , aliasType :: Type                 -- aliased type
        }                                   -- ^ <https://microsoft.github.io/bond/manual/compiler.html#type-aliases type alias definition>
    |
    Service
        { declNamespaces :: [Namespace]     -- namespace(s) in which the service is declared
        , declAttributes :: [Attribute]     -- zero or more attributes
        , declName :: String                -- service name
        , declParams :: [TypeParam]         -- type parameters for generic service
        , serviceBase :: Maybe Type         -- optional base service
        , serviceMethods :: [Method]        -- zero or more methods
        }                                   -- ^ <https://microsoft.github.io/bond/manual/compiler.html#service-definition service definition>
    deriving (Eq, Show)

-- | <https://microsoft.github.io/bond/manual/compiler.html#import-statements Import> declaration.
data Import = Import FilePath
    deriving (Eq, Show)

-- | Language annotation for namespaces. Note that language-specific
-- namespaces are only supported for backward compatibility and are not
-- recommended.
data Language = Cpp | Cs | Java
    deriving (Eq, Show)

-- | <https://microsoft.github.io/bond/manual/compiler.html#namespace-definition Namespace> declaration.
data Namespace =
    Namespace
        { nsLanguage :: Maybe Language
        , nsName :: QualifiedName
        }
    deriving (Eq, Show)

-- | The top level type representing the Bond schema definition abstract syntax tree.
data Bond =
    Bond
        { bondImports :: [Import]
        , bondNamespaces :: [Namespace]
        , bondDeclarations :: [Declaration]
        }
    deriving (Eq, Show)

