-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Bond.Lexer
    ( angles
    , braces
    , brackets
    , colon
    , comma
    , commaEnd
    , commaEnd1
    , commaSep1
    , decimal
    , equal
    , float
    , identifier
    , namespaceIdentifier
    , integer
    , keyword
    , lexeme
    , natural
    , parens
    , unescapedStringLiteral
    , semi
    , semiEnd
    , semiOrCommaEnd
    , semiOrCommaSep
    , semiOrCommaSep1
    , semiOrCommaSepEnd
    , semiOrCommaSepEnd1
    , semiSep
    , stringLiteral
    , symbol
    , whiteSpace
    ) where

import Data.List
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P

type LanguageDef st env = P.GenLanguageDef String st (ReaderT env IO)

bondIdl :: LanguageDef st env
bondIdl = P.LanguageDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "//"
    , P.nestedComments  = True
    , P.identStart      = letter <|> char '_'
    , P.identLetter     = alphaNum <|> char '_'
    , P.opStart         = mzero
    , P.opLetter        = mzero
    , P.reservedNames   =
            [ "blob"
            , "bond_meta"
            , "bonded"
            , "bool"
            , "class"
            , "double"
            , "enum"
            , "false"
            , "float"
            , "import"
            , "int16"
            , "int32"
            , "int64"
            , "int8"
            , "list"
            , "map"
            , "namespace"
            , "nullable"
            , "optional"
            , "required"
            , "required_optional"
            , "Schema"
            , "sealed"
            , "service"
            , "set"
            , "string"
            , "struct"
            , "true"
            , "uint16"
            , "uint32"
            , "uint64"
            , "uint8"
            , "using"
            , "var"
            , "vector"
            , "view_of"
            , "void"
            , "wstring"
            ]
    , P.reservedOpNames = []
    , P.caseSensitive   = True
    }

lexer       = P.makeTokenParser bondIdl

angles      = P.angles lexer
braces      = P.braces lexer
brackets    = P.brackets lexer
colon       = P.colon lexer
comma       = P.comma lexer
commaSep1   = P.commaSep1 lexer
decimal     = P.decimal lexer
identifier  = P.identifier lexer
integer     = P.integer lexer
keyword     = P.reserved lexer
lexeme      = P.lexeme lexer
natural     = P.natural lexer
parens      = P.parens lexer
semi        = P.semi lexer
semiSep     = P.semiSep lexer
symbol      = P.symbol lexer
whiteSpace  = P.whiteSpace lexer

namespaceLexer = P.makeTokenParser bondIdl { P.reservedNames = delete "Schema" (P.reservedNames bondIdl) }
namespaceIdentifier  = P.identifier namespaceLexer

equal       = symbol "="
semiEnd p   = endBy p semi
commaEnd p  = endBy p comma
commaEnd1 p = endBy1 p comma

semiOrComma = semi <|> comma

semiOrCommaSep p     = sepBy p semiOrComma
semiOrCommaSep1 p    = sepBy1 p semiOrComma
semiOrCommaEnd p     = endBy p semiOrComma
semiOrCommaSepEnd p  = sepEndBy p semiOrComma
semiOrCommaSepEnd1 p = sepEndBy1 p semiOrComma

quote = symbol "\""
quotes = between quote quote

stringLiteral = P.stringLiteral lexer

unescapedStringLiteral = quotes $ many $ satisfy (/= '"')

-- Can't use float from Text.Parsec.Token because it doesn't handle numbers
-- starting with +/- sign.
float = do
    s <- sign
    f <- P.float lexer
    return $ s f
  where
    sign = (char '-' >> return negate)
       <|> (char '+' >> return id)
       <|> return id

