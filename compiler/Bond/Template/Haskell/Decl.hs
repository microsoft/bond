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

unqual :: String -> QName
unqual = UnQual . Ident

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

declTypeInfo :: Bond.Template.TypeMapping.Context -> Declaration -> (QualifiedName, String)
declTypeInfo mapping decl = (getDeclNamespace mapping decl, declName decl)

typeParamConstraint :: String -> TypeParam -> Asst
typeParamConstraint className t = ClassA (qualInt className) [TyVar $ mkVar $ paramName t]

stdConstraints :: [TypeParam] -> [Asst]
stdConstraints ts = concatMap paramConstraint ts
    where
    paramConstraint t = [
        typeParamConstraint "FastBinary" t,
        typeParamConstraint "WireType" t,
        typeParamConstraint "Default" t
      ]

declModule :: Bond.Template.TypeMapping.Context -> Declaration -> ModuleName
declModule mapping = uncurry mkModuleName . declTypeInfo mapping

hsType :: Bond.Template.TypeMapping.Context -> Bond.Schema.Type -> Language.Haskell.Exts.Type
hsType _ BT_Int8 = tyInt "Int8"
hsType _ BT_Int16 = tyInt "Int16"
hsType _ BT_Int32 = tyInt "Int32"
hsType _ BT_Int64 = tyInt "Int64"
hsType _ BT_UInt8 = tyInt "Word8"
hsType _ BT_UInt16 = tyInt "Word16"
hsType _ BT_UInt32 = tyInt "Word32"
hsType _ BT_UInt64 = tyInt "Word64"
hsType _ BT_Float = tyInt "Float"
hsType _ BT_Double = tyInt "Double"
hsType _ BT_Bool = tyInt "Bool"
hsType _ BT_String = tyInt "Utf8"
hsType _ BT_WString = tyInt "Utf16"
hsType _ BT_MetaName = error "BT_MetaName" -- tyCon "String"
hsType _ BT_MetaFullName = error "BT_MetaFullName" -- tyCon "String"
hsType _ BT_Blob = tyInt "Blob"
hsType _ (BT_IntTypeArg _) = error "BT_IntTypeArg"
hsType c (BT_Maybe type_) = hsType c (BT_Nullable type_)
hsType c (BT_Nullable element) = TyApp (tyInt "Maybe") (hsType c element)
hsType c (BT_List element) = TyList $ hsType c element
hsType c (BT_Vector element) = TyApp (tyInt "Vector") (hsType c element)
hsType c (BT_Set element) = TyApp (tyInt "HashSet") (hsType c element)
hsType c (BT_Map key value) = TyApp (TyApp (tyInt "Map") (hsType c key)) (hsType c value)
hsType c (BT_Bonded type_) = TyApp (tyInt "Bonded") (hsType c type_)
hsType _ (BT_TypeParam type_) = TyVar $ mkVar $ paramName type_
hsType _ (BT_UserDefined Alias {..} _) = error "BT_UserDefined Alias"
hsType c (BT_UserDefined decl []) = uncurry tyQualCon $ declTypeInfo c decl
hsType c (BT_UserDefined decl params) = TyApp (uncurry tyQualCon $ declTypeInfo c decl) $ foldr1 TyApp $ map (hsType c) params

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
                    importModule = ModuleName "Bond.Imports",
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
                [(unqual "Show", []), (unqual "Eq", []), (unqual "Ord", []), (qualInt "Hashable", [])]
    defaultDecl = defaultInstance mapping e
    wiretypeDecl = wiretypeInstance e
    fastBinaryDecl = fastBinaryInstance e
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
    code = Module noLoc moduleName [] Nothing (Just [EThingAll $ UnQual typeName]) imports decls
    decls = [dataDecl, defaultDecl, wiretypeDecl, fastBinaryDecl, fastBinaryStructDecl, putFieldsTypeSig, putFieldsCode]
    dataDecl = DataDecl noLoc DataType [] typeName typeParams [QualConDecl noLoc [] [] (RecDecl typeName fields)] [(unqual "Show",[]), (unqual "Eq", [])]
    typeParams = map mkTypeParam declParams
    -- FIXME see if type params T and t accepted in C++/C#, make smart conversion to t/t'
    mkTypeParam TypeParam{paramName} = UnkindedVar $ mkVar paramName
    mkField Field{fieldName, fieldType} = ([mkVar fieldName], hsType mapping fieldType)
    ownFields = map mkField structFields
    fields | Just base <- structBase = ([baseStructField], hsType mapping base) : ownFields
           | otherwise = ownFields
    fieldModules = S.fromList $ concatMap (getTypeModules mapping . fieldType) structFields
    modules | Just base <- structBase = foldr S.insert fieldModules (getTypeModules mapping base)
            | otherwise = fieldModules
    imports = defaultImport : map (\m -> importTemplate{importModule = m}) (S.toList $ S.delete moduleName modules)
    defaultDecl = defaultInstance mapping s
    wiretypeDecl = wiretypeInstance s
    fastBinaryDecl = fastBinaryInstance s
    fastBinaryStructDecl = fastBinaryStructInstance s
    putFieldsTypeSig = TypeSig noLoc [Ident "putStruct"] (TyForall Nothing (stdConstraints declParams) (TyFun (makeType False typeName declParams) (TyCon $ qualInt "Put")))
    putFieldsCode = putFieldsImpl s

mkHaskellDecl _ _ = ("/dev/null", "empty")

makeType :: Bool -> Name -> [TypeParam] -> Language.Haskell.Exts.Type
makeType _ typeName [] = TyCon $ UnQual typeName
makeType needParen typeName params
    | needParen = TyParen type'
    | otherwise = type'
    where
    type' = foldl (\v t -> TyApp v (TyVar $ mkVar $ paramName t)) (TyCon $ UnQual typeName) params

defaultInstance :: Bond.Template.TypeMapping.Context -> Declaration -> Decl
defaultInstance _ Enum{declName} = InstDecl noLoc Nothing [] [] (qualInt "Default") [TyCon $ unqual $ convertTypeName declName] [InsDecl $ PatBind noLoc (PVar $ Ident "defaultValue") (UnGuardedRhs $ App (Con $ unqual $ convertTypeName declName) (intLit (0 :: Int))) (BDecls [])]
defaultInstance mapping Struct{declName, declParams, structBase, structFields} = InstDecl noLoc Nothing [] constraints (qualInt "Default") [makeType True (Ident $ convertTypeName declName) declParams] [InsDecl $ PatBind noLoc (PVar $ Ident "defaultValue") (UnGuardedRhs $ RecConstr (unqual $ convertTypeName declName) defaults) (BDecls [])]
    where
    constraints = map (typeParamConstraint "Default") declParams
    fields = map (mkDefaultValue mapping) structFields
    defaults = if isNothing structBase then fields else FieldUpdate (UnQual baseStructField) (Var $ qualInt "defaultValue") : fields
defaultInstance _ _ = error "defaultInstance not implemented"

mkDefaultValue :: Bond.Template.TypeMapping.Context -> Bond.Schema.Field -> FieldUpdate
mkDefaultValue mapping Field{fieldName, fieldType, fieldDefault} = FieldUpdate (UnQual $ mkVar fieldName) (defValue fieldDefault)
    where
    defValue Nothing = Var $ qualInt "defaultValue"
    defValue (Just (DefaultBool v)) = Con $ unqual $ show v
    defValue (Just (DefaultInteger v)) = intLit v
    defValue (Just (DefaultFloat v)) = floatLit v
    defValue (Just (DefaultString v)) = App (Var $ qualInt "fromString") (Lit $ String v)
    defValue (Just (DefaultEnum v)) = let BT_UserDefined decl [] = fieldType
                                       in Var $ Qual (declModule mapping decl) (mkVar v)
    defValue (Just DefaultNothing) = Con $ unqual "Nothing"

getTypeModules :: Bond.Template.TypeMapping.Context -> Bond.Schema.Type -> [ModuleName]
getTypeModules mapping = go
    where
    go (BT_UserDefined decl args) = declModule mapping decl : concatMap go args
    go (BT_Map key value) = go key ++ go value
    go (BT_List element) = go element
    go (BT_Vector element) = go element
    go (BT_Set element) = go element
    go (BT_Nullable element) = go element
    go (BT_Bonded element) = go element
    go _ = []

wiretypeInstance :: Declaration -> Decl
wiretypeInstance Enum{declName} = InstDecl noLoc Nothing [] []
    (qualInt "WireType")
    [TyCon $ unqual $ convertTypeName declName]
    [InsDecl $ FunBind [Match noLoc (Ident "getWireType") [PWildCard] Nothing (UnGuardedRhs (Con $ qualInt "BT_INT32")) (BDecls [])]]
wiretypeInstance Struct{declName, declParams} = InstDecl noLoc Nothing [] []
    (qualInt "WireType")
    [makeType True (Ident $ convertTypeName declName) declParams]
    [InsDecl $ FunBind [Match noLoc (Ident "getWireType") [PWildCard] Nothing (UnGuardedRhs (Con $ qualInt "BT_STRUCT")) (BDecls [])]]
wiretypeInstance _ = error "wiretypeInstance not implemented"

fastBinaryInstance :: Declaration -> Decl
fastBinaryInstance Enum{declName} = InstDecl noLoc Nothing [] []
    (qualInt "FastBinary")
    [TyCon $ unqual $ convertTypeName declName]
    [
        InsDecl $ FunBind [Match noLoc (Ident "fastBinaryPut") [PParen (PApp (unqual $ convertTypeName declName) [PVar $ Ident "v"])] Nothing (UnGuardedRhs (App (Var $ qualInt "putInt32le") (Var $ unqual "v"))) (BDecls [])],
        InsDecl $ PatBind noLoc (PVar $ Ident "fastBinaryGet") (UnGuardedRhs (App (App (Var $ unqual "fmap") (Con $ unqual $ convertTypeName declName)) (Var $ qualInt "getInt32le"))) (BDecls [])
    ]

fastBinaryInstance Struct{declName, declParams, structFields, structBase} = InstDecl noLoc Nothing [] (stdConstraints declParams)
    (qualInt "FastBinary")
    [makeType True (Ident $ convertTypeName declName) declParams]
    [
        InsDecl $ FunBind [Match noLoc (Ident "fastBinaryPut") [PVar recVar] Nothing (UnGuardedRhs $ Do [
            Qualifier (App (Var $ UnQual $ Ident "putStruct") (Var $ UnQual recVar)),
            Qualifier (Var $ qualInt "putStructStop")
          ]) (BDecls [])],
        InsDecl $ PatBind noLoc (PVar $ Ident "fastBinaryGet") (UnGuardedRhs getCode) (BDecls [FunBind updateFuncCode])
      ]
    where
    recVar = Ident "v'"
    fieldVar = Ident "f'"
    typeVar = Ident "t'"
    baseVar = Ident "b'"
    getWithBaseCode = Do [
        Generator noLoc (PVar baseVar) (Var $ qualInt "fastBinaryGet"),
        Qualifier $ App (App (Var $ qualInt "readFieldsWith") (Var $ unqual "update")) (Paren $ RecUpdate (Var $ qualInt "defaultValue") [FieldUpdate (UnQual baseStructField) (Var $ UnQual baseVar)])
      ]
    getNoBaseCode = App (App (Var $ qualInt "readFieldsWith") (Var $ unqual "update")) (Var $ qualInt "defaultValue")
    getCode | isNothing structBase = getNoBaseCode
            | otherwise = getWithBaseCode
    readFunc fieldName fieldOrdinal fieldMod = Match noLoc (Ident "update") [PVar recVar,PVar typeVar,PLit Signless (Int $ fromIntegral fieldOrdinal)] Nothing (UnGuardedRhs $ App (App (Var $ unqual "fmap") (Paren (Lambda noLoc [PVar fieldVar] (RecUpdate (Var $ UnQual recVar) [FieldUpdate (UnQual $ mkVar fieldName) fieldMod])))) (Paren $ App (Var $ qualInt "getField") (Var $ UnQual typeVar))) (BDecls [])
    readField Field{fieldType, fieldName, fieldOrdinal}
        | BT_Maybe _ <- fieldType = readFunc fieldName fieldOrdinal (App (Con $ unqual "Just") (Var $ UnQual fieldVar))
        | otherwise = readFunc fieldName fieldOrdinal (Var $ UnQual fieldVar)
    skipReadCode = Match noLoc (Ident "update") [PVar recVar,PVar typeVar,PWildCard] Nothing (UnGuardedRhs $ Do [
                    Qualifier (App (Var $ qualInt "skipValue") (Var $ UnQual typeVar)),
                    Qualifier (App (Var $ unqual "return") (Var $ UnQual recVar))]) (BDecls [])
    readFieldsCode = map readField structFields
    updateFuncCode = readFieldsCode ++ [skipReadCode]

fastBinaryInstance _ = error "fastBinaryInstance not implemented"

fastBinaryStructInstance :: Declaration -> Decl
fastBinaryStructInstance Struct{declName, declParams} = InstDecl noLoc Nothing [] (stdConstraints declParams)
    (qualInt "FastBinaryStruct")
    [makeType True (Ident $ convertTypeName declName) declParams]
    [
        InsDecl $ FunBind [Match noLoc (Ident "fastBinaryPutBase") [PVar recVar] Nothing (UnGuardedRhs $ Do [
            Qualifier (App (Var $ UnQual $ Ident "putStruct") (Var $ UnQual recVar)),
            Qualifier (Var $ qualInt "putStructStopBase")
          ]) (BDecls [])]
    ]
    where
    recVar = Ident "v'"
fastBinaryStructInstance _ = error "fastBinaryStructInstance not implemented"

putFieldsImpl :: Declaration -> Decl
putFieldsImpl Struct{structFields, structBase} = FunBind [Match noLoc (Ident "putStruct") [PVar recVar] Nothing (UnGuardedRhs (Do putCode)) (BDecls [])]
    where
    recVar = Ident "v'"
    saveFunc fieldName fieldOrdinal func = Qualifier $ App (App (Var $ qualInt func) (intLit fieldOrdinal)) (Paren $ App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar))
    saveUnlessDefaultFunc fieldName fieldOrdinal func = Qualifier $ App (App (Var $ qualInt "unless") (Paren (InfixApp (App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar)) (QVarOp $ UnQual $ Symbol "==") (App (Var $ UnQual $ mkVar fieldName) (Var $ qualInt "defaultValue"))))) (Paren $ App (App (Var $ qualInt func) (intLit fieldOrdinal)) (Paren $ App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar)))
    saveField Field{fieldType, fieldName, fieldOrdinal}
        | BT_Maybe _ <- fieldType = saveFunc fieldName fieldOrdinal "putMaybeField"
        | BT_UserDefined (Struct {}) _ <- fieldType = saveFunc fieldName fieldOrdinal "putField"
        | otherwise = saveUnlessDefaultFunc fieldName fieldOrdinal "putField"
    putFieldsCode = map saveField structFields
    putBaseCode = Qualifier $ App (Var $ qualInt "fastBinaryPutBase") (Paren $ App (Var $ UnQual baseStructField) (Var $ UnQual recVar))
    putCode | isNothing structBase = putFieldsCode
            | otherwise = putBaseCode : putFieldsCode
putFieldsImpl _ = error "putFieldsImpl not implemented"
