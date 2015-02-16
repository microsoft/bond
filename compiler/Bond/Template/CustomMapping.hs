-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings #-}

module Bond.Template.CustomMapping
    ( parseAliasMapping
    , parseNamespaceMapping
    , AliasMapping(..)
    , Fragment(..)
    , NamespaceMapping(..)
    ) where

import Data.Char
import Control.Applicative
import Control.Monad.Identity
import Text.Parsec hiding (many, optional, (<|>))
import Bond.Schema

data Fragment =
    Fragment String |
    Placeholder Int

data AliasMapping = AliasMapping
    { aliasName :: QualifiedName
    , aliasTemplate :: [Fragment]
    }

data NamespaceMapping = NamespaceMapping
    { fromNamespace :: QualifiedName
    , toNamespace :: QualifiedName
    }


whitespace :: ParsecT SourceName u Identity String
whitespace = many (char ' ') <?> "whitespace"
identifier :: ParsecT SourceName u Identity String
identifier = many1 (alphaNum <|> char '_') <?> "identifier"
qualifiedName :: ParsecT SourceName u Identity [String]
qualifiedName = sepBy1 identifier (char '.') <?> "qualified name"
symbol :: String -> ParsecT SourceName u Identity String
symbol s = whitespace *> string s <* whitespace
equal :: ParsecT SourceName u Identity String
equal = symbol "="
integer :: ParsecT SourceName u Identity Integer
integer = decimal <$> many1 digit <?> "decimal number"
  where
    decimal = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0

-- parse alias mapping specification from the command line --using flags
-- e.g.: --using="OrderedSet=SortedSet<{0}>"
parseAliasMapping :: [String] -> IO [AliasMapping]
parseAliasMapping = mapM parseAliasMapping'
  where
    parseAliasMapping' s = case parse aliasMapping s s of
        Left err -> fail $ show err
        Right m -> return m
    aliasMapping = AliasMapping <$> qualifiedName <* equal <*> many1 (placeholder <|> fragment) <* eof
      where
        placeholder = Placeholder <$> fromIntegral <$> between (char '{') (char '}') integer
        fragment = Fragment <$> many1 (noneOf "{")


-- parse namespace mapping specification from the command line --namespace flags
-- e.g.: --namespace="bond="
parseNamespaceMapping :: [String] -> IO [NamespaceMapping]
parseNamespaceMapping = mapM parseNamespaceMapping'
  where
    parseNamespaceMapping' s = case parse namespaceMapping s s of
        Left err -> fail $ show err
        Right m -> return m
    namespaceMapping = NamespaceMapping <$> qualifiedName <* equal <*> qualifiedName
