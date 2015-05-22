-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : alpha
Portability : portable

Types used to specify custom mapping of type aliases and namespaces during code
generation, and parsers for the string specifications of these mappings used in
command line arguments of <https://microsoft.github.io/bond/manual/compiler.html#command-line-options gbc>.
-}

module Language.Bond.Codegen.CustomMapping
    ( -- * Types
      AliasMapping(..)
    , Fragment(..)
    , NamespaceMapping(..)
      -- * Parsers
    , parseAliasMapping
    , parseNamespaceMapping
    ) where

import Data.Char
import Control.Applicative
import Prelude
import Text.Parsec hiding (many, optional, (<|>))
import Language.Bond.Syntax.Types

-- | Specification of a fragment used in type alias mappings.
data Fragment =
    Fragment String |                   -- ^ hardcoded string fragment
    Placeholder Int                     -- ^ placeholder for the n-th type argument of the type alias

-- | Specification of a type alias mapping.
data AliasMapping = AliasMapping
    { aliasName :: QualifiedName        -- ^ type alias qualified name
    , aliasTemplate :: [Fragment]       -- ^ list of fragments
    }

-- | Specification of namespace mapping.
data NamespaceMapping = NamespaceMapping
    { fromNamespace :: QualifiedName    -- ^ namespace used in schema definition
    , toNamespace :: QualifiedName      -- ^ namespece to be used in the generated code
    }

type Parser a = Parsec SourceName () a

whitespace :: Parser String
whitespace = many (char ' ') <?> "whitespace"
identifier :: Parser String
identifier = many1 (alphaNum <|> char '_') <?> "identifier"
qualifiedName :: Parser [String]
qualifiedName = sepBy1 identifier (char '.') <?> "qualified name"
symbol :: String -> Parser String
symbol s = whitespace *> string s <* whitespace
equal :: Parser String
equal = symbol "="
integer :: Parser Integer
integer = decimal <$> many1 digit <?> "decimal number"
  where
    decimal = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0

-- | Parse a type alias mapping specification, e.g.: @"Example.OrderedSet=SortedSet\<{0}\>"@
parseAliasMapping :: String -> Either ParseError AliasMapping 
parseAliasMapping s = parse aliasMapping s s
  where
    aliasMapping = AliasMapping <$> qualifiedName <* equal <*> many1 (placeholder <|> fragment) <* eof
    placeholder = Placeholder <$> fromIntegral <$> between (char '{') (char '}') integer
    fragment = Fragment <$> many1 (noneOf "{")

-- | Parse a namespace mapping specification, e.g.: @"bond=Microsoft.Bond"@
parseNamespaceMapping :: String -> Either ParseError NamespaceMapping
parseNamespaceMapping s = parse namespaceMapping s s
  where
    namespaceMapping = NamespaceMapping <$> qualifiedName <* equal <*> qualifiedName

