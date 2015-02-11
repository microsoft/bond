{-# LANGUAGE PatternGuards, NamedFieldPuns, RecordWildCards #-}

module Bond.Template.Haskell.Decl (mkHaskellDecl) where

import Data.Char
import Data.Maybe
import Data.List (intercalate, mapAccumL)
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import System.FilePath

import qualified Data.Set as S

import Bond.Schema
import Bond.Template.TypeMapping
--import Bond.Template.Util

import Debug.Trace

internalModule :: ModuleName
internalModule = ModuleName "B'"

baseStructField :: Name
baseStructField = Ident "base'"

capitalize :: String -> String
capitalize (h:t) = toUpper h : t
capitalize [] = error "capitalize: empty string"

uncapitalize :: String -> String
uncapitalize (h:t) = toLower h : t
uncapitalize [] = error "uncapitalize: empty string"

mkIdent :: String -> Name
mkIdent = Ident . capitalize

mkVar :: String -> Name
mkVar = Ident . uncapitalize

-- tyCon :: String -> Language.Haskell.Exts.Type
-- tyCon = TyCon . UnQual . mkIdent

tyQualCon :: QualifiedName -> String -> Language.Haskell.Exts.Type
tyQualCon m t = TyCon $ Qual (mkModuleName m t) (mkIdent t)

qualInt :: String -> QName
qualInt = Qual internalModule . Ident

tyInt :: String -> Language.Haskell.Exts.Type
tyInt = TyCon . qualInt

floatLit :: Real a => a -> Exp
floatLit n | n >= 0 = Lit $ Frac $ toRational n
floatLit n = NegApp $ floatLit $ abs n

intLit :: Integral a => a -> Exp
intLit n | n >= 0 = Lit $ Int $ fromIntegral n
intLit n = NegApp $ intLit $ abs n

parenIntLit :: Integral a => a -> Exp
parenIntLit n | n >= 0 = intLit n
parenIntLit n = Paren $ intLit n

declTypeInfo :: Declaration -> (QualifiedName, String)
declTypeInfo decl = (nsName $ head $ declNamespaces decl, declName decl)

declModule :: Declaration -> ModuleName
declModule = uncurry mkModuleName . declTypeInfo

-- C# type mapping
hsType :: Bond.Schema.Type -> Language.Haskell.Exts.Type
hsType BT_Int8 = tyInt "Int8"
hsType BT_Int16 = tyInt "Int16"
hsType BT_Int32 = tyInt "Int32"
hsType BT_Int64 = tyInt "Int64"
hsType BT_UInt8 = tyInt "Word8"
hsType BT_UInt16 = tyInt "Word16"
hsType BT_UInt32 = tyInt "Word32"
hsType BT_UInt64 = tyInt "Word64"
hsType BT_Float = tyInt "Float"
hsType BT_Double = tyInt "Double"
hsType BT_Bool = tyInt "Bool"
hsType BT_String = tyInt "Utf8"
hsType BT_WString = tyInt "Utf16"
hsType BT_MetaName = error "BT_MetaName" -- tyCon "String"
hsType BT_MetaFullName = error "BT_MetaFullName" -- tyCon "String"
hsType BT_Blob = tyInt "Blob"
hsType (BT_IntTypeArg _) = error "BT_IntTypeArg"
hsType (BT_Maybe type_) = hsType (BT_Nullable type_)
hsType (BT_Nullable element) = TyApp (tyInt "Maybe") (hsType element)
hsType (BT_List element) = TyList $ hsType element
hsType (BT_Vector element) = TyApp (tyInt "Vector") (hsType element)
hsType (BT_Set element) = TyApp (tyInt "HashSet") (hsType element)
hsType (BT_Map key value) = TyApp (TyApp (tyInt "Map") (hsType key)) (hsType value)
hsType (BT_Bonded type_) = TyApp (tyInt "Bonded") (hsType type_)
hsType (BT_TypeParam type_) = TyVar $ mkVar $ paramName type_
hsType (BT_UserDefined Alias {..} _) = error "BT_UserDefined Alias"
hsType (BT_UserDefined decl []) = uncurry tyQualCon $ declTypeInfo decl
hsType (BT_UserDefined decl params) = TyApp (uncurry tyQualCon $ declTypeInfo decl) $ foldr1 TyApp $ map hsType params

importTemplate :: ImportDecl
importTemplate = ImportDecl {
                    importLoc = noLoc,
                    importModule = undefined,
                    importQualified = True,
                    importSrc = False,
                    importSafe = False,
                    importPkg = Nothing,
                    importAs = Nothing,
                    importSpecs = Nothing
                }

defaultImport :: ImportDecl
defaultImport = importTemplate {
                    importModule = ModuleName "Bond.Internal",
                    importAs = Just internalModule
                }

convertNamespace :: QualifiedName -> [String]
convertNamespace  = map capitalize

convertTypeName :: String -> String
convertTypeName  = capitalize

mkModuleName :: QualifiedName -> String -> ModuleName
mkModuleName ns t = ModuleName $ intercalate "." $ convertNamespace ns ++ [convertTypeName t]

mkFileName :: QualifiedName -> String -> FilePath
mkFileName ns t = foldr1 (</>) (convertNamespace ns) </> (convertTypeName t ++ ".hs")

mkHaskellDecl :: Bond.Template.TypeMapping.Context -> Declaration -> (FilePath, String)
mkHaskellDecl mapping e@Enum{..} = (filename, prettyPrint code)
    where
    namespace = getIdlNamespace mapping
    filename = mkFileName namespace declName
    moduleName = mkModuleName namespace declName
    typeName = Ident $ convertTypeName declName
    code = Module noLoc moduleName [LanguagePragma noLoc [Ident "GeneralizedNewtypeDeriving"]] Nothing Nothing [defaultImport] decls
    decls = dataDecl : defaultDecl : wiretypeDecl : fastBinaryDecl : typesig : values
    dataDecl = DataDecl noLoc NewType [] typeName []
                [QualConDecl noLoc [] [] (ConDecl typeName [tyInt "Int32"])]
                [(UnQual $ Ident "Show", []), (UnQual $ Ident "Eq", []), (UnQual $ Ident "Ord", []), (qualInt "Hashable", [])]
    defaultDecl = defaultInstance typeName e
    wiretypeDecl = wiretypeInstance typeName e
    fastBinaryDecl = fastBinaryInstance typeName e
    typesig = TypeSig noLoc (map (mkVar . constantName) enumConstants) (TyCon $ UnQual typeName)
    values = let mkval _ Constant{constantName, constantValue = Just i} = (i + 1, (constantName, i))
                 mkval i Constant{constantName} = (i + 1, (constantName, i))
                 constVals = snd $ mapAccumL mkval 0 enumConstants
              in map mkConst constVals
    mkConst (constName, val) = PatBind noLoc (PVar $ mkVar constName) (UnGuardedRhs $ App (Con $ UnQual typeName) (parenIntLit val)) (BDecls [])

mkHaskellDecl mapping s@Struct{..} = traceShow s (filename, prettyPrint code)
    where
    namespace = getIdlNamespace mapping
    filename = mkFileName namespace declName
    moduleName = mkModuleName namespace declName
    typeName = Ident $ convertTypeName declName
    code = Module noLoc moduleName [] Nothing Nothing imports decls
    decls = [dataDecl, defaultDecl, wiretypeDecl, fastBinaryDecl]
    dataDecl = DataDecl noLoc DataType [] typeName typeParams [QualConDecl noLoc [] [] (RecDecl typeName fields)] [(UnQual (Ident "Show"),[])]
    typeParams = map mkTypeParam declParams
    -- FIXME see if type params T and t accepted in C++/C#, make smart conversion to t/t'
    mkTypeParam TypeParam{paramName} = UnkindedVar $ mkVar paramName
    mkField Field{fieldName, fieldType} = ([mkVar fieldName], hsType fieldType)
    ownFields = map mkField structFields
    fields | Just base <- structBase = ([baseStructField], hsType base) : ownFields
           | otherwise = ownFields
    fieldModules = S.fromList $ concatMap (getTypeModules . fieldType) structFields
    modules | Just base <- structBase = foldr S.insert fieldModules (getTypeModules base)
            | otherwise = fieldModules
    imports = defaultImport : map (\m -> importTemplate{importModule = m}) (S.toList $ S.delete moduleName modules)
    defaultDecl = defaultInstance typeName s
    wiretypeDecl = wiretypeInstance typeName s
    fastBinaryDecl = fastBinaryInstance typeName s

mkHaskellDecl _ _ = ("/dev/null", "empty")

makeType :: Name -> [TypeParam] -> Language.Haskell.Exts.Type
makeType typeName [] = TyCon $ UnQual typeName
makeType typeName params = TyParen $ foldl (\v t -> TyApp v (TyVar $ mkVar $ paramName t)) (TyCon $ UnQual typeName) params

defaultInstance :: Name -> Declaration -> Decl
defaultInstance typeName Enum{} = InstDecl noLoc Nothing [] [] (qualInt "Default") [TyCon $ UnQual typeName] [InsDecl $ PatBind noLoc (PVar $ Ident "defaultValue") (UnGuardedRhs $ App (Con $ UnQual typeName) (intLit (0 :: Int))) (BDecls [])]
defaultInstance typeName Struct{declParams, structBase, structFields} = InstDecl noLoc Nothing [] constraints (qualInt "Default") [makeType typeName declParams] [InsDecl $ PatBind noLoc (PVar $ Ident "defaultValue") (UnGuardedRhs $ RecConstr (UnQual typeName) defaults) (BDecls [])]
    where
    constraints = map (\t -> ClassA (qualInt "Default") [TyVar $ mkVar $ paramName t]) declParams
    fields = map mkDefaultValue structFields
    defaults = if isNothing structBase then fields else FieldUpdate (UnQual baseStructField) (Var $ qualInt "defaultValue") : fields
defaultInstance _ _ = error "defaultInstance not implemented"

mkDefaultValue :: Bond.Schema.Field -> FieldUpdate
mkDefaultValue Field{fieldName, fieldType, fieldDefault} = FieldUpdate (UnQual $ mkVar fieldName) (defValue fieldDefault)
    where
    defValue Nothing = Var $ qualInt "defaultValue"
    defValue (Just (DefaultBool v)) = Con $ UnQual $ Ident $ show v
    defValue (Just (DefaultInteger v)) = intLit v
    defValue (Just (DefaultFloat v)) = floatLit v
    defValue (Just (DefaultString v)) = App (Var $ qualInt "fromString") (Lit $ String v)
    defValue (Just (DefaultEnum v)) = let BT_UserDefined decl [] = fieldType
                                       in Var $ Qual (declModule decl) (mkVar v)
    defValue (Just DefaultNothing) = Con $ UnQual $ Ident "Nothing"

getTypeModules :: Bond.Schema.Type -> [ModuleName]
getTypeModules (BT_UserDefined decl args) = declModule decl : concatMap getTypeModules args
getTypeModules (BT_Map key value) = getTypeModules key ++ getTypeModules value
getTypeModules (BT_List element) = getTypeModules element
getTypeModules (BT_Vector element) = getTypeModules element
getTypeModules (BT_Set element) = getTypeModules element
getTypeModules (BT_Nullable element) = getTypeModules element
getTypeModules (BT_Bonded element) = getTypeModules element
getTypeModules _ = []

wiretypeInstance :: Name -> Declaration -> Decl
wiretypeInstance typeName Enum{} = InstDecl noLoc Nothing [] []
    (qualInt "WireType")
    [TyCon $ UnQual typeName]
    [InsDecl $ FunBind [Match noLoc (Ident "getWireType") [PWildCard] Nothing (UnGuardedRhs (Con $ qualInt "BT_INT32")) (BDecls [])]]
wiretypeInstance typeName Struct{declParams} = InstDecl noLoc Nothing [] []
    (qualInt "WireType")
    [makeType typeName declParams]
    [InsDecl $ FunBind [Match noLoc (Ident "getWireType") [PWildCard] Nothing (UnGuardedRhs (Con $ qualInt "BT_STRUCT")) (BDecls [])]]
wiretypeInstance _ _ = error "wiretypeInstance not implemented"

fastBinaryInstance :: Name -> Declaration -> Decl
fastBinaryInstance typeName Enum{} = InstDecl noLoc Nothing [] []
    (qualInt "FastBinary")
    [TyCon $ UnQual typeName]
    [InsDecl (FunBind [Match noLoc (Ident "fastBinaryPut") [PParen (PApp (UnQual typeName) [PVar $ Ident "v"])] Nothing (UnGuardedRhs (App (Var $ qualInt "putInt32le") (Var $ UnQual $ Ident "v"))) (BDecls [])])]

fastBinaryInstance typeName Struct{declParams, structFields, structBase} = InstDecl noLoc Nothing [] constraints
    (qualInt "FastBinary")
    [makeType typeName declParams]
    [InsDecl (FunBind [Match noLoc (Ident "fastBinaryPut") [PVar recVar] Nothing (UnGuardedRhs $ Do code) (BDecls [])])]
    where
    recVar = Ident "v"
    saveField Field{fieldName} = Qualifier $ App (App (Var $ qualInt "putField") (intLit (1 :: Int))) (Paren $ App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar))
    fieldsCode = map saveField structFields
    baseCode = Qualifier $ App (Var $ qualInt "fastBinaryPut") (Paren $ App (Var $ UnQual baseStructField) (Var $ UnQual recVar))
    baseStopCode = Qualifier $ Var $ qualInt "putStopBase"
    code | isNothing structBase = fieldsCode
         | otherwise = baseCode : baseStopCode : fieldsCode
    paramConstraint t = [
        ClassA (qualInt "FastBinary") [TyVar $ mkVar $ paramName t],
        ClassA (qualInt "WireType") [TyVar $ mkVar $ paramName t]
      ]
    constraints = concatMap paramConstraint declParams

fastBinaryInstance _ _ = error "fastBinaryInstance not implemented"
