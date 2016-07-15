-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.ApplyOverloads (applyOverloads, Protocol(..)) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util

-- | Protocol data type is used to specify what protocols the @Apply@ function
-- overloads should be generated for.
data Protocol =
    Protocol
    { protocolReader :: String -- ^ Name of the class implementing the protocol reader.
    , protocolWriter :: String -- ^ Name of the class implementing the protocol writer.
    }


-- Apply overloads
applyOverloads :: [Protocol] -> MappingContext -> Text -> Text -> Declaration -> Text
applyOverloads protocols cpp attr body s@Struct {..} | null declParams = [lt|
    //
    // Overloads of Apply function with common transforms for #{declName}.
    // These overloads will be selected using argument dependent lookup
    // before bond::Apply function templates.
    //
    #{attr}bool Apply(const bond::To< #{qualifiedName}>& transform,
               const bond::bonded< #{qualifiedName}>& value)#{body}

    #{attr}bool Apply(const bond::InitSchemaDef& transform,
               const #{qualifiedName}& value)#{body}
    #{newlineSep 1 applyOverloads' protocols}|]
  where
    qualifiedName = getDeclTypeName cpp s

    applyOverloads' p = [lt|#{deserialization p}
    #{serialization serializer p}
    #{serialization marshaler p}|]

    serializer = "Serializer" :: String
    marshaler = "Marshaler" :: String

    deserialization Protocol {..} = [lt|
    #{attr}bool Apply(const bond::To< #{qualifiedName}>& transform,
               const bond::bonded< #{qualifiedName}, #{protocolReader}&>& value)#{body}

    #{attr}bool Apply(const bond::To< #{qualifiedName}>& transform,
               const bond::bonded<void, #{protocolReader}&>& value)#{body}|]

    serialization transform Protocol {..} = [lt|
    #{attr}bool Apply(const bond::#{transform}<#{protocolWriter} >& transform,
               const #{qualifiedName}& value)#{body}

    #{attr}bool Apply(const bond::#{transform}<#{protocolWriter} >& transform,
               const bond::bonded< #{qualifiedName}>& value)#{body}
    #{newlineSep 1 (transcoding transform) protocols}|]
      where
        transcoding transform' Protocol {protocolReader = fromReader} = [lt|
    #{attr}bool Apply(const bond::#{transform'}<#{protocolWriter} >& transform,
               const bond::bonded< #{qualifiedName}, #{fromReader}&>& value)#{body}|]

applyOverloads _ _ _ _ _ = mempty
