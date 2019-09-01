-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : provisional
Portability : portable

This module provides functionality necessary to parse Bond
<https://microsoft.github.io/bond/manual/compiler.html#idl-syntax schema definition language>.
-}

module Language.Bond.Parser
    ( -- * Parser
      parseBond
    , ImportResolver
    )
    where

import Control.Applicative hiding (some)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Function
import Data.Int
import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Void (Void)
import Data.Word
import Language.Bond.Lexer
import Language.Bond.Syntax.Internal
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Prelude
import Text.Megaparsec hiding (many, optional, (<|>))
import Text.Megaparsec.Char (char)

-- | Parses content of a schema definition file.
parseBond ::
    String                              -- ^ source name, used only for error messages
 -> String                              -- ^ content of a schema file to parse
 -> FilePath                            -- ^ path of the file being parsed, used to resolve relative import paths
 -> ImportResolver                      -- ^ function to resolve and load imported files
 -> IO (Either (ParseErrorBundle String Void) Bond)         -- ^ function returns 'Bond' which represents the parsed abstract syntax tree
                                        --   or 'ParserError' if parsing failed
parseBond s c f r = runReaderT (runParserT (evalStateT bond (Symbols [] [])) s c) (Environment [] [] f r)

-- parser for .bond files
bond :: Parser Bond
bond = do
    whiteSpace
    imports <- many import_
    namespaces <- some namespace
    local (with namespaces) $ Bond imports namespaces <$> many declaration <* eof
  where
    with namespaces e = e { currentNamespaces = namespaces }

import_ :: Parser Import
import_ = do
    i <- Import <$ keyword "import" <*> unescapedStringLiteral <* optional semi <?> "import statement"
    src <- getInput
    pos <- getOffset
    processImport i
    setInput src
    setOffset pos
    return i

processImport :: Import -> Parser()
processImport (Import file) = do
    Environment { currentFile = currentFile, resolveImport = resolveImport } <- ask
    (path, content) <- liftIO $ resolveImport currentFile file
    Symbols { imports = imports } <- get
    if path `elem` imports then return () else do
            modify (\u -> u { imports = path:imports } )
            setInput content
            setSourcePos $ initialPos path
            void $ local (\e -> e { currentFile = path }) bond

-- parser for struct, enum or type alias declaration/definition
declaration :: Parser Declaration
declaration = do
    -- When adding a new Declaration parser, order matters in the following command.
    -- Parsers must fail to consume ANY token for the next parser to be able to successfully work
    -- unless the parser is encapsulated in a try statement. For more info on try and <|> see:
    -- https://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html#v:try
    decl <- try forward
        <|> alias
        <|> (attributes >>= \a -> (service a <|> enum a <|> structDeclaration a))
    updateSymbols decl <?> "declaration"
    return decl

structDeclaration :: [Attribute] -> Parser Declaration
structDeclaration attr = do
    name <- keyword "struct" *> identifier <?> "struct or struct view definition"
    decl <- view attr name <|> struct attr name
    return decl

updateSymbols :: Declaration -> Parser ()
updateSymbols decl = do
    (previous, symbols) <- partition (duplicateDeclaration decl) <$> symbols <$> get
    case reconcile previous decl of
        (False, _) -> fail $ "The " ++ showPretty decl ++ " has been previously defined as " ++ showPretty (head previous)
        (True, f) -> modify (f symbols)
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
    reconcile _   _ = error "updateSymbols/reconcile: impossible happened."
    paramsMatch = (==) `on` (map paramConstraint . declParams)
    add x xs u = u { symbols = x:xs }
    duplicateDeclaration left right =
        (declName left == declName right)
     && not (null $ intersect (declNamespaces left) (declNamespaces right))


findSymbol :: QualifiedName -> Parser Declaration
findSymbol name = doFind <?> "qualified name"
  where
    doFind = do
        namespaces <- asks currentNamespaces
        Symbols { symbols = symbols } <- get
        case find (declMatching namespaces name) symbols of
            Just decl -> return decl
            Nothing -> fail $ "Unknown symbol: " ++ showQualifiedName name
    declMatching namespaces [unqualifiedName] decl =
        unqualifiedName == declName decl
     && (not $ null $ intersectBy nsMatching namespaces (declNamespaces decl))
    declMatching _ qualifiedName' decl =
        takeName qualifiedName' == declName decl
     && any ((takeNamespace qualifiedName' ==) . nsName) (declNamespaces decl)
    nsMatching ns1 ns2 =
        nsName ns1 == nsName ns2 && (lang1 == lang2 || lang1 == Nothing || lang2 == Nothing)
      where
        lang1 = nsLanguage ns1
        lang2 = nsLanguage ns2

findStruct :: QualifiedName -> Parser Declaration
findStruct name = doFind <?> "qualified struct name"
  where
    doFind = do
        symb <- findSymbol name
        case symb of
            Struct {..} -> return symb
            _ -> fail $ "The " ++ showPretty symb ++ " is invalid in this context. Expected a struct."

-- namespace
namespace :: Parser Namespace
namespace = Namespace <$ keyword "namespace" <*> language <*> qualifiedName <* optional semi <?> "namespace declaration"
  where
    language = optional (keyword "cpp" *> pure Cpp
                     <|> keyword "csharp" *> pure Cs
                     <|> keyword "cs" *> pure Cs
                     <|> keyword "java" *> pure Java)

-- identifier optionally qualified with namespace
qualifiedName :: Parser QualifiedName
qualifiedName = sepBy1 namespaceIdentifier (char '.') <?> "qualified name"

-- type parameters
parameters :: Parser [TypeParam]
parameters = option [] (angles $ commaSep1 param) <?> "type parameters"
  where
    param = TypeParam <$> identifier <*> constraint
    constraint = optional (colon *> keyword "value" *> pure Value)

-- type alias
alias :: Parser Declaration
alias = do
    name <- try (keyword "using") *> identifier <?> "alias definition"
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
view :: [Attribute] -> String -> Parser Declaration
view attr name = do
    decl <- try (keyword "view_of") *> qualifiedName >>= findStruct <?> "struct view definition"
    fields <- braces $ semiOrCommaSepEnd1 identifier
    namespaces <- asks currentNamespaces
    Struct namespaces attr name (declParams decl) (structBase decl) (viewFields decl fields) <$ optional semi
  where
    viewFields Struct {..} fields = filter ((`elem` fields) . fieldName) structFields
    viewFields _           _      = error "view/viewFields: impossible happened."

-- struct definition parser
struct :: [Attribute] -> String -> Parser Declaration
struct attr name = do
    params <- parameters
    namespaces <- asks currentNamespaces
    updateSymbols $ Forward namespaces name params
    local (with params) $ Struct namespaces attr name params <$> base <*> fields <* optional semi
  where
    base = optional (colon *> userType <?> "base struct")
    fields = sortFields $ unique $ braces $ many (field <* semi)
    with params e = e { currentParams = params }
    sortFields p = do
        fields' <- p
        return $ sortBy (comparing fieldOrdinal) fields'
    unique p = do
        fields' <- p
        case findDuplicatesBy fieldOrdinal fields' ++ findDuplicatesBy fieldName fields' of
            [] -> return fields'
            Field {..}:_ -> fail $ "Duplicate definition of the field with ordinal " ++ show fieldOrdinal ++
                " and name " ++ show fieldName

-- field definition parser
field :: Parser Field
field = do
    mf <- makeField <$> attributes <*> ordinal <*> modifier <*> ftype <*> identifier <*> optional default_
    case mf of
      Left e -> fail e
      Right f -> return f
  where
    ordinal = word16 <* colon <?> "field ordinal"
      where
        word16 = do
            i <- integer
            if isInBounds i (0::Word16)
                then return (fromInteger i)
                else fail "Field ordinal must be within the range 0-65535"
    modifier = option Optional
                    (keyword "optional" *> pure Optional
                 <|> keyword "required_optional" *> pure RequiredOptional
                 <|> keyword "required" *> pure Required)
    default_ = equal *>
                    (keyword "true" *> pure (DefaultBool True)
                 <|> keyword "false" *> pure (DefaultBool False)
                 <|> keyword "nothing" *> pure DefaultNothing
                 <|> DefaultString <$> try (optional (char 'L') *> stringLiteral)
                 <|> DefaultEnum <$> identifier
                 <|> DefaultFloat <$> try float
                 <|> DefaultInteger <$> fromIntegral <$> integer)
    makeField a o m t n d@(Just DefaultNothing)
        | isStruct t = Left "Struct field can't have default value of 'nothing'"
        | otherwise  = Right $ Field a o m (BT_Maybe t) n d
    makeField a o m t n d
        | d == Nothing && isEnum t = Left "Enum field must have a default value"
        | otherwise                = if validDefaultType t d
                                        then Right $ Field a o m t n d
                                        else Left "Invalid default value for field"

-- enum definition parser
enum :: [Attribute] -> Parser Declaration
enum attr = Enum <$> asks currentNamespaces <*> pure attr <*> name <*> consts <* optional semi <?> "enum definition"
  where
    name = try (keyword "enum") *> identifier <?> "enum identifier"
    consts = braces (semiOrCommaSepEnd1 constant <?> "enum constant")
    constant = Constant <$> identifier <*> optional value
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

-- containers parser
complexType :: Parser Type
complexType =
        keyword "list" *> angles (BT_List <$> type_)
    <|> keyword "blob" *> pure BT_Blob
    <|> keyword "vector" *> angles (BT_Vector <$> type_)
    <|> keyword "nullable" *> angles (BT_Nullable <$> type_)
    <|> keyword "set" *> angles (BT_Set <$> keyType)
    <|> keyword "map" *> angles (BT_Map <$> keyType <* comma <*> type_)
    <|> keyword "bonded" *> angles (BT_Bonded <$> userStructRef)
  where
    keyType = try (basicType <|> checkUserType isValidKeyType) <?> "scalar, string or enum"
    isValidKeyType t = isScalar t || isString t

-- parser for user defined type (struct, enum, alias or type parameter)
userType :: Parser Type
userType = do
    symbol_ <- userSymbol
    case symbol_ of
        Left param -> return $ BT_TypeParam param
        Right (Service {..}, _) -> fail $ "Unexpected service " ++ declName ++ ". Expected struct, enum or alias."
        Right (decl, args) -> return $ BT_UserDefined decl args

-- parser for service type
serviceType :: Parser Type
serviceType = do
    symbol_ <- userSymbol
    case symbol_ of
        Right (decl@Service{}, args) -> return $ BT_UserDefined decl args
        Right (decl, _) -> fail $ "Unexpected type " ++ (declName decl) ++ ". Expected a service."
        Left param -> fail $ "Unexpected type parameter " ++ (paramName param) ++ ". Expected a service."

userSymbol :: Parser (Either TypeParam (Declaration, [Type]))
userSymbol = do
    name <- qualifiedName
    params <- asks currentParams
    case find (isParam name) params of
        Just param -> return $ Left param
        Nothing -> do
            decl <- findSymbol name
            args <- option [] (angles $ commaSep1 arg)
            if length args /= paramsCount decl then
                fail $ declName decl ++
                    if paramsCount decl /= 0 then
                        " requires " ++ show (paramsCount decl) ++ " type argument(s)"
                    else
                        " is not a generic type"
                else
                    return $ Right (decl, args)
          where
            paramsCount Enum{} = 0
            paramsCount decl   = length $ declParams decl
            arg = type_ <|> BT_IntTypeArg <$> (fromIntegral <$> integer)
  where
    isParam [name] = (name ==) . paramName
    isParam _      = const False



-- type parser
type_ :: Parser Type
type_ = (try basicType) <|> (try complexType) <|> (try userType)

-- field type parser
ftype :: Parser Type
ftype = keyword "bond_meta::name" *> pure BT_MetaName
    <|> keyword "bond_meta::full_name" *> pure BT_MetaFullName
    <|> type_

-- service definition parser
service :: [Attribute] -> Parser Declaration
service attr = do
    name <- try (keyword "service") *> identifier <?> "service definition"
    params <- parameters
    namespaces <- asks currentNamespaces
    local (with params) $ Service namespaces attr name params <$> base <*> methods <* optional semi
  where
    base = optional (colon *> serviceType <?> "base service")
    with params e = e { currentParams = params }
    methods = checkUniqueMethodNames $ braces $ semiEnd method
    checkUniqueMethodNames p = do
        methods' <- p
        case findDuplicatesBy methodName methods' of
            [] -> return methods'
            Function {..}:_ -> fail $ "Duplicate definition of the function with name " ++ show methodName
            Event {..}:_ -> fail $ "Duplicate definition of the event with name " ++ show methodName

method :: Parser Method
method = attributes >>= \a -> ((lookAhead (keyword "nothing") *> event a) <|> function a)

function :: [Attribute] -> Parser Method
function attr = Function attr <$> functionResultType <*> identifier <*> input
  where functionResultType = methodTypeVoid  <|> methodResultTypeStreaming <|> methodTypeUnary

event :: [Attribute] -> Parser Method
event attr = do
  _ <- keyword "nothing"
  methodName <- identifier
  methodInput <- input
  case methodInput of
    (Streaming _) -> fail $ "Incompatible nothing return and streaming input in method " ++ show methodName
    _ -> return (Event attr methodName methodInput)

input :: Parser MethodType
input = parens methodInputType
  where methodInputType = (fromMaybe Void) <$> optional (methodTypeVoid <|> methodInputTypeStreaming <|> methodTypeUnary)

methodTypeVoid :: Parser MethodType
methodTypeVoid = try (keyword "void" *> pure Void) <?> "void method type"

-- Whether the method type is streaming or is unary can be determined based on
-- context, but the context is different for result and input types.
--
-- For result types, the keyword stream followed by a struct name AND THEN
-- an identifier indicates a streaming type. Two identifiers are required to
-- distinguish between the unary method "stream stream()" and the streaming
-- method "stream stream stream()".
--
-- For input types, simply the keyword stream followed by a struct name is
-- enough to distinguish between the unary "foo(stream)" and the streaming
-- "foo(stream stream)".
methodResultTypeStreaming :: Parser MethodType
methodResultTypeStreaming = try (do
                                    _ <- keyword "stream"
                                    resultType <- userStructRef
                                    _ <- lookAhead identifier
                                    return (Streaming resultType)) <?> "streaming method type"

methodInputTypeStreaming :: Parser MethodType
methodInputTypeStreaming = try (Streaming <$ keyword "stream" <*> userStructRef) <?> "streaming method type"

methodTypeUnary :: Parser MethodType
methodTypeUnary = (Unary <$> userStructRef) <?> "unary method type"

-- helper methods

checkUserType :: (Type -> Bool) -> Parser Type
checkUserType check = do
    t <- userType
    if (valid t) then return t else fail "unexpected type"
  where
    valid t = case t of
        BT_TypeParam _ -> True
        _ -> check t

userStructRef :: Parser Type
userStructRef = try (checkUserType isStruct) <?> "user defined struct reference"

findDuplicatesBy :: (Eq b) => (a -> b) -> [a] -> [a]
findDuplicatesBy accessor xs = deleteFirstsBy ((==) `on` accessor) xs (nubBy ((==) `on` accessor) xs)

-- default type validator (type checking, out-of-range, enforce default type)
validDefaultType :: Type -> Maybe Default -> Bool
validDefaultType (BT_UserDefined a@Alias {} args) d = validDefaultType (resolveAlias a args) d
validDefaultType _ Nothing = True
validDefaultType bondType (Just defaultValue) = validDefaultType' bondType defaultValue
  where validDefaultType' :: Type -> Default -> Bool
        validDefaultType' BT_Int8    (DefaultInteger i) = isInBounds i (0::Int8)
        validDefaultType' BT_Int16   (DefaultInteger i) = isInBounds i (0::Int16)
        validDefaultType' BT_Int32   (DefaultInteger i) = isInBounds i (0::Int32)
        validDefaultType' BT_Int64   (DefaultInteger i) = isInBounds i (0::Int64)
        validDefaultType' BT_UInt8   (DefaultInteger i) = isInBounds i (0::Word8)
        validDefaultType' BT_UInt16  (DefaultInteger i) = isInBounds i (0::Word16)
        validDefaultType' BT_UInt32  (DefaultInteger i) = isInBounds i (0::Word32)
        validDefaultType' BT_UInt64  (DefaultInteger i) = isInBounds i (0::Word64)
        validDefaultType' BT_Float   (DefaultFloat _)   = True
        validDefaultType' BT_Float   (DefaultInteger _) = True
        validDefaultType' BT_Double  (DefaultFloat _)   = True
        validDefaultType' BT_Double  (DefaultInteger _) = True
        validDefaultType' BT_Bool    (DefaultBool _)    = True
        validDefaultType' BT_String  (DefaultString _)  = True
        validDefaultType' BT_WString (DefaultString _)  = True
        validDefaultType' (BT_UserDefined Enum {} _) (DefaultEnum _) = True
        validDefaultType' (BT_TypeParam {}) _           = True
        validDefaultType' _ _                           = False

-- checks whether an Integer is within the bounds of some other Integral and Bounded type.
-- The value of the second parameter is never used: only its type is used.
isInBounds :: forall a. (Integral a, Bounded a) => Integer -> a -> Bool
isInBounds value _ = value >= (toInteger (minBound :: a)) && value <= (toInteger (maxBound :: a))

-- sets source position
setSourcePos ::  MonadParsec e s m => SourcePos -> m ()
setSourcePos src = updateParserState setPos
    where setPos (State s o (PosState i o' _ t l)) =  State s o (PosState i o' src t l)
