-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Bond.Template.Cpp.Apply_h (apply_h, Protocol(..), applyOverloads) where

import System.FilePath
import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Bond.Schema.Types
import Bond.Util
import Bond.Template.Util
import Bond.Template.TypeMapping
import qualified Bond.Template.Cpp.Util as CPP

data Protocol =
    Protocol
    { protocolReader :: String
    , protocolWriter :: String
    }

-- generate the *_apply.h file from parsed .bond file
apply_h :: [Protocol] -> Maybe String -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
apply_h protocols attribute cpp file imports declarations = ("_apply.h", [lt|
#pragma once

#include "#{file}_types.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#{newlineSep 0 includeImport imports}

#{CPP.openNamespace cpp}
    #{newlineSepEnd 1 (applyOverloads protocols attr semi) declarations}
#{CPP.closeNamespace cpp}
|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension path}_apply.h"|]

    attr = optional (\a -> [lt|#{a}
    |]) attribute

    semi = [lt|;|]

-- Apply overloads
applyOverloads :: [Protocol] -> Text -> Text -> Declaration -> Text
applyOverloads protocols attr body Struct {..} | null declParams = [lt|
    //
    // Overloads of Apply function with common transforms for #{declName}.
    // These overloads will be selected using argument dependent lookup
    // before bond::Apply function templates.
    //
    #{attr}bool Apply(const bond::To<#{declName}>& transform,
               const bond::bonded<#{declName}>& value)#{body}

    #{attr}bool Apply(const bond::InitSchemaDef& transform,
               const #{declName}& value)#{body}
    #{newlineSep 1 applyOverloads' protocols}|]
  where
    applyOverloads' p = [lt|#{deserialization p}
    #{serialization serializer p}
    #{serialization marshaler p}|]

    serializer = "Serializer" :: String
    marshaler = "Marshaler" :: String

    deserialization Protocol {..} = [lt|
    #{attr}bool Apply(const bond::To<#{declName}>& transform,
               const bond::bonded<#{declName}, bond::#{protocolReader}<bond::InputBuffer>&>& value)#{body}

    #{attr}bool Apply(const bond::To<#{declName}>& transform,
               const bond::bonded<void, bond::#{protocolReader}<bond::InputBuffer>&>& value)#{body}|]

    serialization transform Protocol {..} = [lt|
    #{attr}bool Apply(const bond::#{transform}<bond::#{protocolWriter}<bond::OutputBuffer> >& transform,
               const #{declName}& value)#{body}

    #{attr}bool Apply(const bond::#{transform}<bond::#{protocolWriter}<bond::OutputBuffer> >& transform,
               const bond::bonded<#{declName}>& value)#{body}
    #{newlineSep 1 (transcoding transform) protocols}|]
      where
        transcoding transform' Protocol {protocolReader = fromReader} = [lt|
    #{attr}bool Apply(const bond::#{transform'}<bond::#{protocolWriter}<bond::OutputBuffer> >& transform,
               const bond::bonded<#{declName}, bond::#{fromReader}<bond::InputBuffer>&>& value)#{body}|]

applyOverloads _ _ _ _ = mempty
