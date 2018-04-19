-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Language.Bond.Syntax.Internal
    ( showPretty
    , showQualifiedName
    , takeName
    , takeNamespace
    , isBaseField
    , metaField
    , baseFields
    ) where

import Data.Monoid
import Language.Bond.Util
import Language.Bond.Syntax.Util
import Language.Bond.Syntax.Types

takeName :: QualifiedName -> String
takeName = last

takeNamespace :: QualifiedName -> QualifiedName
takeNamespace = subtract 1 . length >>= take

showQualifiedName :: QualifiedName -> String
showQualifiedName = sepBy "." id

showTypeParams :: [TypeParam] -> String
showTypeParams = angles . sepBy ", " showPretty

class ShowPretty a where
    showPretty :: a -> String

instance ShowPretty Constraint where
    showPretty Value = ": value"

instance ShowPretty TypeParam where
    showPretty TypeParam {..} = paramName ++ optional showPretty paramConstraint

instance ShowPretty Declaration where
    showPretty Struct {..} = "struct " ++ declName ++ showTypeParams declParams
    showPretty Enum {..} = "enum " ++ declName
    showPretty Forward {..} = "struct declaration " ++ declName ++ showTypeParams declParams
    showPretty Alias {..} = "alias " ++ declName ++ showTypeParams declParams
    showPretty Service {..} = "service " ++ declName ++ showTypeParams declParams

metaField :: Field -> Any
metaField Field {..} = Any $ isMetaName fieldType

isBaseField :: String -> Maybe Type -> Bool
isBaseField name = getAny . optional (foldMapFields (Any.(name==).fieldName))

-- If a Declaration is a Struct then return the Fields of its Parent
baseFields :: Declaration -> Maybe [Field]
baseFields Struct{..} = foldMapFields return <$> structBase
baseFields _ = Nothing
