-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}

module Bond.Parser 
    ( Bond(..)
    , parseBond
    , newEnvironment
    ) 
    where

import Data.Ord
import Data.List
import Data.Function
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Text.Parsec.Pos (initialPos)
import Text.Parsec hiding (many, optional, (<|>))
import Bond.Lexer
import Bond.Schema

-- parser state, mutable and global
-- list of structs, enums and aliases declared in the current and all imported files
type Symbols = [Declaration]

-- parser environment, immutable but contextual
data Environment =
    Environment
    { currentNamespaces :: [Namespace]  -- namespace(s) in current context
    , currentParams :: [TypeParam]      -- type parameter(s) for current type (struct or alias)
    , currentFile :: FilePath           -- path of the current file
    , resolveImport :: FilePath -> FilePath -> IO (FilePath, String) -- imports resolver 
    }

newEnvironment :: FilePath -> (FilePath -> FilePath -> IO (FilePath, String)) -> Environment
newEnvironment = Environment [] []

type Parser a = ParsecT String Symbols (ReaderT Environment IO) a

parseBond = runParserT bond []

data Bond = Bond [Import] [Namespace] [Declaration]

-- parser for .bond files
bond :: Parser Bond
bond = do
    whiteSpace
    imports <- many import_
    namespaces <- many1 namespace
    local (with namespaces) $ Bond imports namespaces <$> many declaration <* eof
  where
    with namespaces e = e { currentNamespaces = namespaces }

import_ :: Parser Import
import_ = do
    i <- Import <$ keyword "import" <*> unescapedStringLiteral <?> "import statement"
    input <- getInput
    pos <- getPosition
    processImport i
    setInput input
    setPosition pos
    return i

processImport :: Import -> Parser Bond
processImport (Import file) = do
    Environment { currentFile = currentFile, resolveImport = resolveImport } <- ask
    (path, content) <- liftIO $ resolveImport currentFile file
    setInput content
    setPosition $ initialPos path
    local (\e -> e { currentFile = path }) bond

-- parser for struct, enum or type alias declaration/definition
declaration :: Parser Declaration
declaration = do
    decl <- try forward
        <|> try struct
        <|> try view
        <|> try enum
        <|> try alias
    updateSymbols decl <?> "declaration"
    return decl

updateSymbols decl = do
    (previous, symbols) <- partition (duplicateDeclaration decl) <$> getState
    case reconcile previous decl of
        (False, _) -> fail $ "The " ++ show decl ++ " has been previously defined as " ++ show (head previous)
        (True, f) -> modifyState (f symbols)
  where
    reconcile [x@Forward {}] y@Struct {} = (paramsMatch x y, add y)
    reconcile [x@Forward {}] y@Forward {} = (paramsMatch x y, const id)
    reconcile [x@Struct {}] y@Forward {} = (paramsMatch x y, add y)
    reconcile [] x = (True, add x)
    -- This allows identical duplicate definitions, which is how parsing the
    -- same import multiple times is handled. Ideally we would avoid parsing
    -- imports multiple times but that would have to depend on canonical file
    -- paths which are unreliable.
    reconcile [x] y = (x == y, const id)
    paramsMatch = (==) `on` (map paramConstraint . declParams)
    add x xs _ = x:xs

findSymbol :: QualifiedName -> Parser Declaration
findSymbol name = doFind <?> "qualified name"
  where
    doFind = do
        namespaces <- asks currentNamespaces
        symbols <- getState
        case find (isDecl namespaces name) symbols of
            Just decl -> return decl
            Nothing -> fail $ "Unknown symbol: " ++ showQualifiedName name
    isDecl namespaces [name] decl =
        name == declName decl
     && any (`elem` namespaces) (declNamespaces decl)
    isDecl _ qualifiedName decl =
        takeName qualifiedName == declName decl
     && any ((takeNamespace qualifiedName ==) . nsName) (declNamespaces decl)

findStruct :: QualifiedName -> Parser Declaration
findStruct name = doFind <?> "qualified struct name"
  where
    doFind = do
        symbol <- findSymbol name
        case symbol of
            Struct {..} -> return symbol
            _ -> fail $ "The " ++ show symbol ++ " is invalid in this context. Expected a struct."

-- namespace
namespace :: Parser Namespace
namespace = Namespace <$ keyword "namespace" <*> language <*> qualifiedName <* optional semi <?> "namespace declaration"
  where
    language = optional (keyword "cpp" *> pure Cpp
                     <|> keyword "cs" *> pure Cs
                     <|> keyword "java" *> pure Java
                     <|> keyword "csharp" *> pure Cs)

-- identifier optionally qualified with namespace
qualifiedName :: Parser QualifiedName
qualifiedName = sepBy1 identifier (char '.') <?> "qualified name"

-- type parameters
parameters :: Parser [TypeParam]
parameters = option [] (angles $ commaSep1 param) <?> "type parameters"
  where
    param = TypeParam <$> identifier <*> constraint
    constraint = optional (colon *> keyword "value" *> pure Value)

-- type alias
alias :: Parser Declaration
alias = do
    name <- keyword "using" *> identifier <?> "alias definition"
    params <- parameters
    namespaces <- asks currentNamespaces
    local (with params) $ Alias namespaces name params <$ equal <*> type_ <* semi
  where
    with params e = e { currentParams = params }

-- forward declaration
forward :: Parser Declaration
forward = Forward <$> asks currentNamespaces <*> name <*> parameters <* semi <?> "forward declaration"
  where
    name = keyword "struct" *> identifier

-- attributes parser
attributes :: Parser [Attribute]
attributes = many attribute <?> "attributes"
  where
    attribute = brackets (Attribute <$> qualifiedName <*> parens stringLiteral <?> "attribute")

-- struct view parser
view :: Parser Declaration
view = do
    attr <- attributes
    name <- keyword "struct" *> identifier
    decl <- keyword "view_of" *> qualifiedName >>= findStruct
    fields <- braces $ semiOrCommaSepEnd1 identifier
    namespaces <- asks currentNamespaces
    Struct namespaces attr name (declParams decl) (structBase decl) (viewFields decl fields) <$ optional semi
  where
    viewFields Struct {..} fields = filter ((`elem` fields) . fieldName) structFields

-- struct definition parser
struct :: Parser Declaration
struct = do
    attr <- attributes
    name <- keyword "struct" *> identifier <?> "struct definition"
    params <- parameters
    namespaces <- asks currentNamespaces
    updateSymbols $ Forward namespaces name params
    local (with params) $ Struct namespaces attr name params <$> base <*> fields <* optional semi
  where
    base = optional (colon *> userType <?> "base struct")
    fields = unique $ braces $ manySortedBy (comparing fieldOrdinal) (field <* semi)
    with params e = e { currentParams = params }
    unique p = do
        fields <- p
        case findDuplicates fields of
            [] -> return fields
            Field {..}:_ -> fail $ "Duplicate definition of the field with ordinal " ++ show fieldOrdinal
      where
        findDuplicates xs = deleteFirstsBy ordinal xs (nubBy ordinal xs)
        ordinal = (==) `on` fieldOrdinal

manySortedBy = manyAccum . insertBy

-- field definition parser
field :: Parser Field
field = makeField <$> attributes <*> ordinal <*> modifier <*> ftype <*> identifier <*> optional default_
  where
    ordinal = (fromIntegral <$> integer) <* colon <?> "field ordinal"
    modifier = option Optional
                    (keyword "optional" *> pure Optional
                 <|> keyword "required" *> pure Required
                 <|> keyword "required_optional" *> pure RequiredOptional)
    default_ = equal *>
                    (keyword "true" *> pure (DefaultBool True)
                 <|> keyword "false" *> pure (DefaultBool False)
                 <|> keyword "nothing" *> pure DefaultNothing
                 <|> DefaultString <$> try (optional (char 'L') *> stringLiteral)
                 <|> DefaultEnum <$> identifier
                 <|> DefaultFloat <$> try float
                 <|> DefaultInteger <$> fromIntegral <$> integer)

-- enum definition parser
enum :: Parser Declaration
enum = Enum <$> asks currentNamespaces <*> attributes <*> name <*> consts <* optional semi <?> "enum definition"
  where
    name = keyword "enum" *> (identifier <?> "enum identifier")
    consts = braces (semiOrCommaSepEnd1 const <?> "enum constant")
    const = Constant <$> identifier <*> optional value
    value = equal *> (fromIntegral <$> integer)

-- basic types parser
basicType :: Parser Type
basicType =
        keyword "int8" *> pure BT_Int8
    <|> keyword "int16" *> pure BT_Int16
    <|> keyword "int32" *> pure BT_Int32
    <|> keyword "int64" *> pure BT_Int64
    <|> keyword "uint8" *> pure BT_UInt8
    <|> keyword "uint16" *> pure BT_UInt16
    <|> keyword "uint32" *> pure BT_UInt32
    <|> keyword "uint64" *> pure BT_UInt64
    <|> keyword "float" *> pure BT_Float
    <|> keyword "double" *> pure BT_Double
    <|> keyword "wstring" *> pure BT_WString
    <|> keyword "string" *> pure BT_String
    <|> keyword "bool" *> pure BT_Bool

keyType :: Parser Type
keyType = try (basicType <|> basicUserType) <?> "scalar, string or enum"

basicUserType :: Parser Type
basicUserType = do
    t <- userType
    if isBasic t then
            return t
        else
            fail $ "Disallowed key type: " ++ typeName t
  where
    isBasic t = case t of
        BT_TypeParam _ -> True
        BT_UserDefined a@Alias {..} params -> isBasic $ resolveAlias a params
        BT_String -> True
        BT_WString -> True
        _ -> scalarType t
    typeName (BT_UserDefined decl _) = declName decl

-- containers parser
complexType :: Parser Type
complexType =
        keyword "list" *> angles (BT_List <$> type_)
    <|> keyword "blob" *> pure BT_Blob
    <|> keyword "vector" *> angles (BT_Vector <$> type_)
    <|> keyword "nullable" *> angles (BT_Nullable <$> type_)
    <|> keyword "set" *> angles (BT_Set <$> keyType)
    <|> keyword "map" *> angles (BT_Map <$> keyType <* comma <*> type_)
    <|> keyword "bonded" *> angles (BT_Bonded <$> userType)

--  parser for user defined type (struct, enum, alias or type parameter)
userType :: Parser Type
userType = do
    name <- qualifiedName
    params <- asks currentParams
    case find (isParam name) params of
        Just param -> return $ BT_TypeParam param
        Nothing -> do
            decl <- findSymbol name
            args <- option [] (angles $ commaSep1 arg)
            if length args /= length (params decl)
                then
                    fail $ declName decl ++ " requires " ++ (show.length $ declParams decl) ++ " type argument(s)"
                else
                    return $ BT_UserDefined decl args
          where
            params Enum{..} = []
            params d = declParams d
            arg = type_ <|> BT_IntTypeArg <$> (fromIntegral <$> integer)
  where
    isParam [name] TypeParam {..} = name == paramName
    isParam _ _ = False

-- type parser
type_ :: Parser Type
type_ = try (basicType <|> complexType <|> userType) <?> "type"

-- field type parser
ftype :: Parser Type
ftype = keyword "bond_meta::name" *> pure BT_MetaName
    <|> keyword "bond_meta::full_name" *> pure BT_MetaFullName
    <|> type_

