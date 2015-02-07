{-# LANGUAGE PatternGuards, NamedFieldPuns, RecordWildCards #-}

module Bond.Template.Haskell.Decl (mkHaskellDecl) where

import Data.Char
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

tyInt :: String -> Language.Haskell.Exts.Type
tyInt = TyCon . Qual internalModule . mkIdent

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
mkHaskellDecl mapping Enum{..} = (filename, prettyPrint code)
    where
    namespace = getIdlNamespace mapping
    filename = mkFileName namespace declName
    moduleName = mkModuleName namespace declName
    typeName = Ident $ convertTypeName declName
    code = Module noLoc moduleName [] Nothing Nothing [] decls
    decls = datadecl : typesig : values
    datadecl = DataDecl noLoc NewType [] typeName []
                [QualConDecl noLoc [] [] (ConDecl typeName [TyCon $ UnQual $ Ident "Int"])]
                [(UnQual $ Ident "Show", []), (UnQual $ Ident "Eq", []), (UnQual $ Ident "Ord", [])]
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
    decls = datadecl : []
    datadecl = DataDecl noLoc DataType [] typeName typeParams [QualConDecl noLoc [] [] (RecDecl typeName fields)] [(UnQual (Ident "Show"),[])]
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

mkHaskellDecl _ _ = ("/dev/null", "empty")

getTypeModules :: Bond.Schema.Type -> [ModuleName]
getTypeModules (BT_UserDefined decl args) = mkModuleName (nsName $ head $ declNamespaces decl) (declName decl) : concatMap getTypeModules args
getTypeModules (BT_Map key value) = getTypeModules key ++ getTypeModules value
getTypeModules (BT_List element) = getTypeModules element
getTypeModules (BT_Vector element) = getTypeModules element
getTypeModules (BT_Set element) = getTypeModules element
getTypeModules (BT_Nullable element) = getTypeModules element
getTypeModules (BT_Bonded element) = getTypeModules element
getTypeModules _ = []
