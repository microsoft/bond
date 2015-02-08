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

tyCon :: String -> Language.Haskell.Exts.Type
tyCon = TyCon . UnQual . mkIdent

qualInt :: String -> QName
qualInt = Qual internalModule . Ident

tyInt :: String -> Language.Haskell.Exts.Type
tyInt = TyCon . qualInt

intLit :: Integral a => a -> Exp
intLit n | n >= 0 = Lit $ Int $ fromIntegral n
intLit n = Paren $ NegApp $ intLit $ abs n

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
hsType BT_Float = tyCon "Float"
hsType BT_Double = tyCon "Double"
hsType BT_Bool = tyCon "Bool"
hsType BT_String = tyInt "ByteString"
hsType BT_WString = tyInt "ByteString"
hsType BT_MetaName = error "BT_MetaName" -- tyCon "String"
hsType BT_MetaFullName = error "BT_MetaFullName" -- tyCon "String"
hsType BT_Blob = tyInt "ByteString"
hsType (BT_IntTypeArg _) = error "BT_IntTypeArg"
hsType (BT_Maybe type_) = hsType (BT_Nullable type_)
hsType (BT_Nullable element) = TyApp (tyCon "Maybe") (hsType element)
hsType (BT_List element) = TyList $ hsType element
hsType (BT_Vector element) = TyApp (tyInt "Vector") (hsType element)
hsType (BT_Set element) = TyApp (tyInt "HashSet") (hsType element)
hsType (BT_Map key value) = TyApp (TyApp (tyInt "Map") (hsType key)) (hsType value)
hsType (BT_Bonded type_) = TyApp (tyInt "Bonded") (hsType type_)
hsType (BT_TypeParam type_) = TyVar $ mkVar $ paramName type_
hsType (BT_UserDefined Alias {..} _) = error "BT_UserDefined Alias"
hsType (BT_UserDefined decl []) = tyCon $ declName decl
hsType (BT_UserDefined decl params) = TyApp (tyCon $ declName decl) $ foldr1 TyApp $ map hsType params

importTemplate :: ImportDecl
importTemplate = ImportDecl {
                    importLoc = noLoc,
                    importModule = undefined,
                    importQualified = False,
                    importSrc = False,
                    importSafe = False,
                    importPkg = Nothing,
                    importAs = Nothing,
                    importSpecs = Nothing
                }

defaultImport :: ImportDecl
defaultImport = importTemplate {
                    importModule = ModuleName "Bond.Internal",
                    importQualified = True,
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

--declModule :: Declaration -> ModuleName
--declModule t = mkModuleName (nsName $ head $ declNamespaces t) (declName t)

mkHaskellDecl :: Bond.Template.TypeMapping.Context -> Declaration -> (FilePath, String)
mkHaskellDecl mapping e@Enum{..} = (filename, prettyPrint code)
    where
    namespace = getIdlNamespace mapping
    filename = mkFileName namespace declName
    moduleName = mkModuleName namespace declName
    typeName = Ident $ convertTypeName declName
    code = Module noLoc moduleName [] Nothing Nothing [defaultImport] decls
    decls = dataDecl : defaultDecl : typesig : values
    dataDecl = DataDecl noLoc NewType [] typeName []
                [QualConDecl noLoc [] [] (ConDecl typeName [TyCon $ UnQual $ Ident "Int"])]
                [(UnQual $ Ident "Show", []), (UnQual $ Ident "Eq", []), (UnQual $ Ident "Ord", [])]
    defaultDecl = defaultInstance typeName e
    typesig = TypeSig noLoc (map (mkVar . constantName) enumConstants) (TyCon $ UnQual typeName)
    values = let mkval _ Constant{constantName, constantValue = Just i} = (i + 1, (constantName, i))
                 mkval i Constant{constantName} = (i + 1, (constantName, i))
                 constVals = snd $ mapAccumL mkval 0 enumConstants
              in map mkConst constVals
    mkConst (constName, val) = PatBind noLoc (PVar $ mkVar constName) (UnGuardedRhs $ App (Con $ UnQual typeName) (intLit val)) (BDecls [])

mkHaskellDecl mapping s@Struct{..} = traceShow s (filename, prettyPrint code)
    where
    namespace = getIdlNamespace mapping
    filename = mkFileName namespace declName
    moduleName = mkModuleName namespace declName
    typeName = Ident $ convertTypeName declName
    code = Module noLoc moduleName [] Nothing Nothing imports decls
    decls = [dataDecl, defaultDecl]
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
    imports = defaultImport : map (\(m, sp) -> importTemplate{importModule = m, importSpecs = Just (False, [sp])}) (filter (\(m, _) -> m /= moduleName) $ S.toList modules)
    defaultDecl = defaultInstance typeName s

mkHaskellDecl _ _ = ("/dev/null", "empty")

defaultInstance :: Name -> Declaration -> Decl
defaultInstance typeName Enum{} = InstDecl noLoc Nothing [] [] (qualInt "Default") [TyCon $ UnQual typeName] [InsDecl $ PatBind noLoc (PVar $ Ident "defaultValue") (UnGuardedRhs $ App (Con $ UnQual typeName) (intLit (0 :: Int))) (BDecls [])]
defaultInstance typeName Struct{declParams, structBase, structFields} = InstDecl noLoc Nothing [] constraints (qualInt "Default") [typeId] [InsDecl $ PatBind noLoc (PVar $ Ident "defaultValue") (UnGuardedRhs $ RecConstr (UnQual typeName) defaults) (BDecls [])]
    where
    constraints = map (\t -> ClassA (qualInt "Default") [TyVar $ mkVar $ paramName t]) declParams
    typeExpr = foldl (\v t -> TyApp v (TyVar $ mkVar $ paramName t)) (TyCon $ UnQual typeName) declParams
    typeId = if null declParams then typeExpr else TyParen typeExpr
    fields = map mkDefaultValue structFields
    defaults = if isNothing structBase then fields else FieldUpdate (UnQual baseStructField) (Var $ qualInt "defaultValue") : fields
defaultInstance _ _ = error "defaultInstance not implemented"

mkDefaultValue :: Bond.Schema.Field -> FieldUpdate
mkDefaultValue Field{..} | isNothing fieldDefault = FieldUpdate (UnQual $ mkVar fieldName) (Var $ qualInt "defaultValue")
mkDefaultValue Field{..} = FieldUpdate (UnQual $ mkVar fieldName) (Var $ qualInt "defaultValue")

getTypeModules :: Bond.Schema.Type -> [(ModuleName, ImportSpec)]
getTypeModules (BT_UserDefined decl args) = let modname = mkModuleName (nsName $ head $ declNamespaces decl) (declName decl) 
                                                spec = IAbs $ mkIdent $ declName decl
                                             in (modname, spec) : concatMap getTypeModules args
getTypeModules (BT_Map key value) = getTypeModules key ++ getTypeModules value
getTypeModules (BT_List element) = getTypeModules element
getTypeModules (BT_Vector element) = getTypeModules element
getTypeModules (BT_Set element) = getTypeModules element
getTypeModules (BT_Nullable element) = getTypeModules element
getTypeModules (BT_Bonded element) = getTypeModules element
getTypeModules _ = []
