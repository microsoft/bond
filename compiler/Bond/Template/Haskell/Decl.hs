{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Bond.Template.Haskell.Decl (mkHaskellDecl) where

import Data.Char
import Data.List (sort, group, intercalate, mapAccumL)
import Data.Maybe
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import System.FilePath

import Bond.Schema
import Bond.Template.TypeMapping
--import Bond.Template.Util

import Debug.Trace

capitalize :: String -> String
capitalize s = (toUpper $ head s) : tail s

uncapitalize :: String -> String
uncapitalize s = (toLower $ head s) : tail s

mkIdent :: String -> Name
mkIdent = Ident . capitalize

mkVar :: String -> Name
mkVar = Ident . uncapitalize

tyCon :: String -> Language.Haskell.Exts.Type
tyCon = TyCon . UnQual . mkIdent

intLit :: Integral a => a -> Exp
intLit n | n >= 0 = Lit $ Int $ fromIntegral n
intLit n = Paren $ NegApp $ intLit $ abs n

-- C# type mapping
hsType :: Bond.Schema.Type -> Language.Haskell.Exts.Type
hsType BT_Int8 = tyCon "Int8"
hsType BT_Int16 = tyCon "Int16"
hsType BT_Int32 = tyCon "Int32"
hsType BT_Int64 = tyCon "Int64"
hsType BT_UInt8 = tyCon "Word8"
hsType BT_UInt16 = tyCon "Word16"
hsType BT_UInt32 = tyCon "Word32"
hsType BT_UInt64 = tyCon "Word64"
hsType BT_Float = tyCon "Float"
hsType BT_Double = tyCon "Double"
hsType BT_Bool = tyCon "Bool"
hsType BT_String = tyCon "ByteString"
hsType BT_WString = tyCon "ByteString"
hsType BT_MetaName = tyCon "String"
hsType BT_MetaFullName = tyCon "String"
hsType BT_Blob = tyCon "ByteString"
hsType (BT_IntTypeArg _) = error "BT_IntTypeArg"
hsType (BT_Maybe type_) = hsType (BT_Nullable type_)
hsType (BT_Nullable element) = TyApp (tyCon "Maybe") (hsType element)
hsType (BT_List element) = TyList $ hsType element
hsType (BT_Vector element) = TyApp (tyCon "Vector") (hsType element)
hsType (BT_Set element) = TyApp (tyCon "HashSet") (hsType element)
hsType (BT_Map key value) = TyApp (TyApp (tyCon "Map") (hsType key)) (hsType value)
hsType (BT_Bonded type_) = TyApp (tyCon "Bonded") (hsType type_)
hsType (BT_TypeParam type_) = TyVar $ mkVar $ paramName type_
hsType (BT_UserDefined Alias {..} _) = error "BT_UserDefined Alias"
hsType (BT_UserDefined decl []) = tyCon $ declName decl
hsType (BT_UserDefined decl params) = TyApp (tyCon $ declName decl) $ foldr1 TyApp $ map hsType params

defaultImport :: ImportDecl
defaultImport = ImportDecl {
                    importLoc = noLoc,
                    importModule = ModuleName "Bond.Internal",
                    importQualified = False,
                    importSrc = False,
                    importSafe = False,
                    importPkg = Nothing,
                    importAs = Nothing,
                    importSpecs = Nothing
                }

convertNamespace :: QualifiedName -> [String]
convertNamespace  = map capitalize

convertTypeName :: String -> String
convertTypeName  = capitalize

mkModuleName :: QualifiedName -> String -> ModuleName
mkModuleName ns t = ModuleName $ intercalate "." $ (convertNamespace ns) ++ [convertTypeName t]

mkFileName :: QualifiedName -> String -> FilePath
mkFileName ns t = (foldr1 (</>) $ convertNamespace ns) </> (convertTypeName t ++ ".hs")

--declModule :: Declaration -> ModuleName
--declModule t = mkModuleName (nsName $ head $ declNamespaces t) (declName t)

mkHaskellDecl :: Bond.Template.TypeMapping.Context -> Declaration -> (FilePath, String)
mkHaskellDecl mapping Enum{..} = (filename, prettyPrint code)
    where
    namespace = getIdlNamespace mapping
    filename = mkFileName namespace declName
    moduleName = mkModuleName namespace declName
    typeName = Ident $ convertTypeName declName
    code = Module noLoc moduleName [] Nothing Nothing [defaultImport] decls
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

mkHaskellDecl mapping s@Struct{..} = traceShow s $ (filename, prettyPrint code)
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
    mkTypeParam TypeParam{..} = UnkindedVar $ mkVar paramName
    fields = map mkField structFields
    mkField Field{..} = ([mkVar fieldName], hsType fieldType)
    types = map head $ group $ sort $ foldMapStructFields extractTypes s
    imports = defaultImport : mapMaybe (\m -> if m == moduleName then Nothing else Just defaultImport{importModule = m}) types

mkHaskellDecl _ _ = ("/dev/null", "empty")

extractTypes :: Field -> [ModuleName]
extractTypes f = foldMapType go $ fieldType f
    where
    go (BT_UserDefined d _) = [mkModuleName (nsName $ head $ declNamespaces d) (declName d)]
    go _ = []
