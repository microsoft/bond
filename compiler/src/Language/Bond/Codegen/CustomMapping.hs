-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings #-}

module Language.Bond.Codegen.CustomMapping
    ( AliasMapping(..)
    , Fragment(..)
    , NamespaceMapping(..)
    , parseAliasMapping
    , parseNamespaceMapping
    ) where

import Control.Applicative hiding (some)
import Data.Void (Void)
import Language.Bond.Syntax.Types
import Prelude
import Text.Megaparsec hiding (many, optional, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Specification of a fragment of type alias mappings.
data Fragment =
    Fragment String |                   -- ^ hardcoded string fragment
    Placeholder Int                     -- ^ placeholder for the n-th type argument of the type alias, applicable only to generic aliases

-- | Specification of a type alias mapping.
data AliasMapping = AliasMapping
    { aliasName :: QualifiedName        -- ^ qualified name of a type alias
    , aliasTemplate :: [Fragment]       -- ^ list of fragments comprising the custom mapping for the alias
    }

-- | Specification of namespace mapping.
data NamespaceMapping = NamespaceMapping
    { fromNamespace :: QualifiedName    -- ^ schema namespace
    , toNamespace :: QualifiedName      -- ^ namespace in the generated code
    }

type Parser = Parsec Void String

identifier :: Parser String
identifier = some (alphaNumChar <|> char '_') <?> "identifier"

qualifiedName :: Parser [String]
qualifiedName = sepBy1 identifier (char '.') <?> "qualified name"

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

equal :: Parser String
equal = symbol "="

-- consume whitespace after every lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme . try $ char '0' >> char' 'x' >> L.hexadecimal

octal :: Parser Integer
octal = lexeme . try $ char '0' >> char' 'o' >> L.octal

natural :: Parser Integer
natural = hexadecimal <|> octal <|> decimal

integer :: Parser Integer
integer = L.signed sc natural

-- | Parse a type alias mapping specification used in command-line arguments of
-- <https://microsoft.github.io/bond/manual/compiler.html#command-line-options gbc>.
--
-- ==== __Examples__
--
-- > > parseAliasMapping "Example.OrderedSet=SortedSet<{0}>"
-- > Right (AliasMapping {aliasName = ["Example","OrderedSet"], aliasTemplate = [Fragment "SortedSet<",Placeholder 0,Fragment ">"]})
parseAliasMapping :: String -> Either (ParseErrorBundle String Void) AliasMapping
parseAliasMapping s = parse aliasMapping "" s
  where
    aliasMapping = AliasMapping <$> qualifiedName <* equal <*> some (placeholder <|> fragment) <* eof
    placeholder = Placeholder <$> fromIntegral <$> between (char '{') (char '}') integer
    fragment = Fragment <$> some (anySingleBut '{')

-- | Parse a namespace mapping specification used in command-line arguments of
-- <https://microsoft.github.io/bond/manual/compiler.html#command-line-options gbc>.
--
-- ==== __Examples__
--
-- > > parseNamespaceMapping "bond=Microsoft.Bond"
-- > Right (NamespaceMapping {fromNamespace = ["bond"], toNamespace = ["Microsoft","Bond"]})
parseNamespaceMapping :: String -> Either (ParseErrorBundle String Void) NamespaceMapping
parseNamespaceMapping s = parse namespaceMapping "" s
  where
    namespaceMapping = NamespaceMapping <$> qualifiedName <* equal <*> qualifiedName

