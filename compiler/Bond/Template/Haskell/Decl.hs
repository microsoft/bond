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
 
internalModule :: ModuleName
internalModule = ModuleName "B'"
 
baseStructField :: Name
baseStructField = Ident "base'"
 
capitalize :: String -> String
capitalize (h : t) = toUpper h : t
capitalize [] = error "capitalize: empty string"
 
uncapitalize :: String -> String
uncapitalize (h : t) = toLower h : t
uncapitalize [] = error "uncapitalize: empty string"
 
mkIdent :: String -> Name
mkIdent = Ident . capitalize
 
mkVar :: String -> Name
mkVar = Ident . uncapitalize
 
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
 
declTypeInfo ::
             MappingContext -> Declaration -> (QualifiedName, String)
declTypeInfo mapping decl
  = (getDeclNamespace mapping decl, declName decl)
 
typeParamConstraint :: String -> TypeParam -> Asst
typeParamConstraint className t
  = ClassA (qualInt className) [TyVar $ mkVar $ paramName t]
 
stdConstraints :: [TypeParam] -> [Asst]
stdConstraints = concatMap paramConstraint
  where paramConstraint t
          = [ClassA (qualInt "BondBinary") [TyVar $ mkVar $ paramName t],
             typeParamConstraint "WireType" t, typeParamConstraint "Default" t]
 
declModule :: MappingContext -> Declaration -> ModuleName
declModule mapping = uncurry mkModuleName . declTypeInfo mapping
 
hsType ::
       MappingContext -> Bond.Schema.Type -> Language.Haskell.Exts.Type
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
hsType _ BT_MetaName = error "BT_MetaName"
hsType _ BT_MetaFullName = error "BT_MetaFullName"
hsType _ BT_Blob = tyInt "Blob"
hsType _ (BT_IntTypeArg _) = error "BT_IntTypeArg"
hsType c (BT_Maybe type_) = hsType c (BT_Nullable type_)
hsType c (BT_Nullable element)
  = TyApp (tyInt "Maybe") (hsType c element)
hsType c (BT_List element) = TyList $ hsType c element
hsType c (BT_Vector element)
  = TyApp (tyInt "Vector") (hsType c element)
hsType c (BT_Set element)
  = TyApp (tyInt "HashSet") (hsType c element)
hsType c (BT_Map key value)
  = TyApp (TyApp (tyInt "Map") (hsType c key)) (hsType c value)
hsType c (BT_Bonded type_)
  = TyApp (tyInt "Bonded") (hsType c type_)
hsType _ (BT_TypeParam type_) = TyVar $ mkVar $ paramName type_
hsType _ (BT_UserDefined Alias{..} _)
  = error "BT_UserDefined Alias"
hsType c (BT_UserDefined decl [])
  = uncurry tyQualCon $ declTypeInfo c decl
hsType c (BT_UserDefined decl params)
  = TyApp (uncurry tyQualCon $ declTypeInfo c decl) $
      foldr1 TyApp $ map (hsType c) params
 
wireType :: Bond.Schema.Type -> QName
wireType BT_Int8 = qualInt "BT_INT8"
wireType BT_Int16 = qualInt "BT_INT16"
wireType BT_Int32 = qualInt "BT_INT32"
wireType BT_Int64 = qualInt "BT_INT64"
wireType BT_UInt8 = qualInt "BT_UINT8"
wireType BT_UInt16 = qualInt "BT_UINT16"
wireType BT_UInt32 = qualInt "BT_UINT32"
wireType BT_UInt64 = qualInt "BT_UINT64"
wireType BT_Float = qualInt "BT_FLOAT"
wireType BT_Double = qualInt "BT_DOUBLE"
wireType BT_Bool = qualInt "BT_BOOL"
wireType BT_String = qualInt "BT_STRING"
wireType BT_WString = qualInt "BT_WSTRING"
wireType BT_MetaName = error "BT_MetaName"
wireType BT_MetaFullName = error "BT_MetaFullName"
wireType BT_Blob = qualInt "BT_LIST"
wireType (BT_IntTypeArg _) = error "BT_IntTypeArg"
wireType (BT_Maybe t) = wireType t
wireType (BT_Nullable _) = qualInt "BT_LIST"
wireType (BT_List _) = qualInt "BT_LIST"
wireType (BT_Vector _) = qualInt "BT_LIST"
wireType (BT_Set _) = qualInt "BT_SET"
wireType (BT_Map _ _) = qualInt "BT_MAP"
wireType (BT_Bonded t) = wireType t
wireType (BT_TypeParam _) = error "BT_TypeParam"
wireType (BT_UserDefined Enum{} _) = qualInt "BT_INT32"
wireType (BT_UserDefined _ _) = qualInt "BT_STRUCT"
 
importTemplate :: ImportDecl
importTemplate
  = ImportDecl{importLoc = noLoc, importModule = undefined,
               importQualified = True, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
 
defaultImport :: ImportDecl
defaultImport
  = importTemplate{importModule = ModuleName "Bond.Imports",
                   importAs = Just internalModule}
 
convertNamespace :: QualifiedName -> [String]
convertNamespace = map capitalize
 
convertTypeName :: String -> String
convertTypeName = capitalize
 
mkModuleName :: QualifiedName -> String -> ModuleName
mkModuleName ns t
  = ModuleName $
      intercalate "." $ convertNamespace ns ++ [convertTypeName t]
 
mkFileName :: QualifiedName -> String -> FilePath
mkFileName ns t
  = foldr1 (</>) (convertNamespace ns) </>
      (convertTypeName t ++ ".hs")
 
mkHaskellDecl ::
              MappingContext -> Declaration -> (FilePath, String)
mkHaskellDecl mapping e@Enum{..} = (filename, prettyPrint code)
  where namespace = getIdlNamespace mapping
        filename = mkFileName namespace declName
        moduleName = mkModuleName namespace declName
        typeName = Ident $ convertTypeName declName
        code
          = Module noLoc moduleName
              [LanguagePragma noLoc
                 [Ident "GeneralizedNewtypeDeriving", Ident "DeriveDataTypeable"]]
              Nothing
              Nothing
              [defaultImport]
              decls
        decls
          = dataDecl :
              defaultDecl : wiretypeDecl : bondBinaryDecl : typesig : values
        dataDecl
          = DataDecl noLoc NewType [] typeName []
              [QualConDecl noLoc [] [] (ConDecl typeName [tyInt "Int32"])]
              [(unqual "Show", []), (unqual "Eq", []), (unqual "Ord", []),
               (qualInt "Hashable", []), (qualInt "Data", []),
               (qualInt "Typeable", [])]
        defaultDecl = defaultInstance mapping e
        wiretypeDecl = wiretypeInstance e
        bondBinaryDecl = bondBinaryInstance e
        typesig
          = TypeSig noLoc (map (mkVar . constantName) enumConstants)
              (TyCon $ UnQual typeName)
        values
          = let mkval _ Constant{constantName, constantValue = Just i}
                  = (i + 1, (constantName, i))
                mkval i Constant{constantName} = (i + 1, (constantName, i))
                constVals = snd $ mapAccumL mkval 0 enumConstants
              in map mkConst constVals
        mkConst (constName, val)
          = PatBind noLoc (PVar $ mkVar constName)
              (UnGuardedRhs $ App (Con $ UnQual typeName) (parenIntLit val))
              (BDecls [])
mkHaskellDecl mapping s@Struct{..} = (filename, prettyPrint code)
  where namespace = getIdlNamespace mapping
        filename = mkFileName namespace declName
        moduleName = mkModuleName namespace declName
        typeName = Ident $ convertTypeName declName
        code
          = Module noLoc moduleName
              [LanguagePragma noLoc
                 [Ident "ScopedTypeVariables", Ident "TypeFamilies",
                  Ident "DeriveDataTypeable"]]
              Nothing
              (Just [EThingAll $ UnQual typeName])
              imports
              decls
        decls
          = [dataDecl, defaultDecl, wiretypeDecl, bondBinaryDecl,
             bondStructDecl]
        dataDecl
          = DataDecl noLoc DataType [] typeName typeParams
              [QualConDecl noLoc [] [] (RecDecl typeName fields)]
              [(unqual "Show", []), (unqual "Eq", []), (qualInt "Data", []),
               (qualInt "Typeable", [])]
        typeParams = map mkTypeParam declParams
        mkTypeParam TypeParam{paramName} = UnkindedVar $ mkVar paramName
        mkField Field{fieldName, fieldType}
          = ([mkVar fieldName], hsType mapping fieldType)
        ownFields = map mkField structFields
        fields
          | Just base <- structBase =
            ([baseStructField], hsType mapping base) : ownFields
          | otherwise = ownFields
        fieldModules
          = S.fromList $
              concatMap (getTypeModules mapping . fieldType) structFields
        modules
          | Just base <- structBase =
            foldr S.insert fieldModules (getTypeModules mapping base)
          | otherwise = fieldModules
        imports
          = defaultImport :
              map (\ m -> importTemplate{importModule = m})
                (S.toList $ S.delete moduleName modules)
        defaultDecl = defaultInstance mapping s
        wiretypeDecl = wiretypeInstance s
        bondBinaryDecl = bondBinaryInstance s
        bondStructDecl = bondStructInstance mapping s
mkHaskellDecl _ _ = ("/dev/null", "empty")
 
makeType ::
         Bool -> Name -> [TypeParam] -> Language.Haskell.Exts.Type
makeType _ typeName [] = TyCon $ UnQual typeName
makeType needParen typeName params
  | needParen = TyParen type'
  | otherwise = type'
  where type'
          = foldl (\ v t -> TyApp v (TyVar $ mkVar $ paramName t))
              (TyCon $ UnQual typeName)
              params
 
defaultInstance :: MappingContext -> Declaration -> Decl
defaultInstance _ Enum{declName}
  = InstDecl noLoc Nothing [] [] (qualInt "Default")
      [TyCon $ unqual $ convertTypeName declName]
      [InsDecl $
         PatBind noLoc (PVar $ Ident "defaultValue")
           (UnGuardedRhs $
              App (Con $ unqual $ convertTypeName declName) (intLit (0 :: Int)))
           (BDecls []),
       InsDecl $
         PatBind noLoc (PVar $ Ident "equalToDefault")
           (UnGuardedRhs $ Var $ UnQual $ Symbol "==")
           (BDecls [])]
defaultInstance mapping
  Struct{declName, declParams, structBase, structFields}
  = InstDecl noLoc Nothing [] constraints (qualInt "Default")
      [makeType True (Ident $ convertTypeName declName) declParams]
      [InsDecl $
         PatBind noLoc (PVar $ Ident "defaultValue")
           (UnGuardedRhs $
              RecConstr (unqual $ convertTypeName declName) defaults)
           (BDecls []),
       InsDecl $
         FunBind
           [Match noLoc (Ident "equalToDefault") [PWildCard, PWildCard]
              Nothing
              (UnGuardedRhs $ Con $ unqual "False")
              (BDecls [])]]
  where constraints = map (typeParamConstraint "Default") declParams
        fields = map (mkDefaultValue mapping) structFields
        defaults
          = if isNothing structBase then fields else
              FieldUpdate (UnQual baseStructField) (Var $ qualInt "defaultValue")
                : fields
defaultInstance _ _ = error "defaultInstance not implemented"
 
mkDefaultValue ::
               MappingContext -> Bond.Schema.Field -> FieldUpdate
mkDefaultValue mapping Field{fieldName, fieldType, fieldDefault}
  = FieldUpdate (UnQual $ mkVar fieldName) (defValue fieldDefault)
  where defValue Nothing = Var $ qualInt "defaultValue"
        defValue (Just (DefaultBool v)) = Con $ unqual $ show v
        defValue (Just (DefaultInteger v)) = intLit v
        defValue (Just (DefaultFloat v)) = floatLit v
        defValue (Just (DefaultString v))
          = App (Var $ qualInt "fromString") (Lit $ String v)
        defValue (Just (DefaultEnum v))
          = let BT_UserDefined decl [] = fieldType in
              Var $ Qual (declModule mapping decl) (mkVar v)
        defValue (Just DefaultNothing) = Con $ unqual "Nothing"
 
getTypeModules ::
               MappingContext -> Bond.Schema.Type -> [ModuleName]
getTypeModules mapping = go
  where go (BT_UserDefined decl args)
          = declModule mapping decl : concatMap go args
        go (BT_Map key value) = go key ++ go value
        go (BT_List element) = go element
        go (BT_Vector element) = go element
        go (BT_Set element) = go element
        go (BT_Nullable element) = go element
        go (BT_Bonded element) = go element
        go _ = []
 
wiretypeInstance :: Declaration -> Decl
wiretypeInstance Enum{declName}
  = InstDecl noLoc Nothing [] [] (qualInt "WireType")
      [TyCon $ unqual $ convertTypeName declName]
      [InsDecl $
         FunBind
           [Match noLoc (Ident "getWireType") [PWildCard] Nothing
              (UnGuardedRhs (Con $ qualInt "BT_INT32"))
              (BDecls [])]]
wiretypeInstance Struct{declName, declParams}
  = InstDecl noLoc Nothing [] [] (qualInt "WireType")
      [makeType True (Ident $ convertTypeName declName) declParams]
      [InsDecl $
         FunBind
           [Match noLoc (Ident "getWireType") [PWildCard] Nothing
              (UnGuardedRhs (Con $ qualInt "BT_STRUCT"))
              (BDecls [])]]
wiretypeInstance _ = error "wiretypeInstance not implemented"
 
bondBinaryInstance :: Declaration -> Decl
bondBinaryInstance Enum{declName}
  = InstDecl noLoc Nothing [] [] (qualInt "BondBinary")
      [TyCon $ unqual $ convertTypeName declName]
      [InsDecl $
         FunBind
           [Match noLoc (Ident "bondPut")
              [PParen
                 (PApp (unqual $ convertTypeName declName) [PVar $ Ident "v"])]
              Nothing
              (UnGuardedRhs $
                 App (Var $ qualInt "bondPutInt32") (Var $ unqual "v"))
              (BDecls [])],
       InsDecl $
         PatBind noLoc (PVar $ Ident "bondGet")
           (UnGuardedRhs $
              InfixApp
                (InfixApp (Var $ unqual "return") (QVarOp $ UnQual $ Symbol ".")
                   (App (Var $ unqual "fmap")
                      (Con $ unqual $ convertTypeName declName)))
                (QVarOp $ UnQual $ Symbol "=<<")
                (Var $ qualInt "bondGet"))
           (BDecls [])]
bondBinaryInstance Struct{declName, declParams}
  = InstDecl noLoc Nothing [] (stdConstraints declParams)
      (qualInt "BondBinary")
      [makeType True (Ident $ convertTypeName declName) declParams]
      [InsDecl $
         PatBind noLoc (PVar $ Ident "bondPut")
           (UnGuardedRhs $ Var $ qualInt "bondPutStruct")
           (BDecls []),
       InsDecl $
         PatBind noLoc (PVar $ Ident "bondGet")
           (UnGuardedRhs $ Var $ qualInt "bondGetStruct")
           (BDecls [])]
bondBinaryInstance _ = error "bondBinaryInstance not implemented"
 
bondStructInstance :: MappingContext -> Declaration -> Decl
bondStructInstance mapping decl@Struct{}
  = InstDecl noLoc Nothing [] (stdConstraints (declParams decl))
      (qualInt "BondStruct")
      [typeId]
      [InsType noLoc (TyApp (TyCon $ unqual "Base") typeId) baseId,
       InsDecl
         (FunBind
            [Match noLoc (Ident "bondGetInfo") [PWildCard] Nothing
               (UnGuardedRhs
                  (App
                     (App (Con $ qualInt "StructInfo")
                        (Paren $
                           ExpTypeSig noLoc (Con $ qualInt "Proxy")
                             (TyApp (TyCon $ qualInt "Proxy") typeId)))
                     baseProxy))
               (BDecls [])]),
       InsDecl $
         FunBind
           [Match noLoc (Ident "bondGetSchema") [PWildCard] Nothing
              (UnGuardedRhs $
                 App (Con $ qualInt "StructSchema")
                   (List $ map makeFieldSchema (structFields decl)))
              (BDecls [])],
       InsDecl (FunBind [setBase]), InsDecl setFieldFunc, InsDecl getBase,
       InsDecl checkField, InsDecl putField]
  where typeId
          = makeType True (Ident $ convertTypeName (declName decl))
              (declParams decl)
        baseId
          | Just base <- structBase decl = hsType mapping base
          | otherwise = TyCon $ qualInt "VoidBase"
        baseProxy
          | Just base <- structBase decl =
            Paren $
              App (Con $ unqual "Just")
                (Paren $
                   ExpTypeSig noLoc (Con $ qualInt "Proxy")
                     (TyApp (TyCon $ qualInt "Proxy") (hsType mapping base)))
          | otherwise = Con $ unqual "Nothing"
        recVar = Ident "v'"
        baseVar = Ident "b'"
        valueVar = Ident "val'"
        ordVar = Ident "ord'"
        typeVar = Ident "type'tag"
        setFuncName = Ident "bondSetField"
        setFieldFunc
          | length (structFields decl) < 20 =
            FunBind $
              map (makeSetFunc setFuncName) (structFields decl) ++
                [defaultSetFunc setFuncName]
          | otherwise =
            PatBind noLoc (PVar $ Ident "bondSetField")
              (UnGuardedRhs $ Var $ unqual "setField0")
              (BDecls $ setBondDecls (0 :: Int) (structFields decl))
        setBondDecls _ [] = []
        setBondDecls idx fields
          = let funcName = Ident $ "setField" ++ show idx
                nextName = Ident $ "setField" ++ show (idx + 1)
                (fs, rest) = splitAt 20 fields
                lastDecl
                  | null rest = defaultSetFunc funcName
                  | otherwise =
                    Match noLoc funcName [PVar ordVar, PVar typeVar] Nothing
                      (UnGuardedRhs $
                         App (App (Var $ UnQual nextName) (Var $ UnQual ordVar))
                           (Var $ UnQual typeVar))
                      (BDecls [])
                code = FunBind $ map (makeSetFunc funcName) fs ++ [lastDecl]
              in code : setBondDecls (idx + 1) rest
        makeFieldSchema
          Field{fieldOrdinal, fieldName, fieldType = BT_TypeParam t}
          = App
              (App
                 (App (Con $ qualInt "FieldInfo")
                    (Lit $ String $ uncapitalize fieldName))
                 (Paren $
                    App (Var $ qualInt "getWireType")
                      (Paren $
                         ExpTypeSig noLoc (Con $ qualInt "Proxy")
                           (TyApp (TyCon $ qualInt "Proxy") (TyVar $ mkVar $ paramName t)))))
              (Paren $ App (Con $ qualInt "Ordinal") (intLit fieldOrdinal))
        makeFieldSchema Field{fieldOrdinal, fieldName, fieldType}
          = App
              (App
                 (App (Con $ qualInt "FieldInfo")
                    (Lit $ String $ uncapitalize fieldName))
                 (Con $ wireType fieldType))
              (Paren $ App (Con $ qualInt "Ordinal") (intLit fieldOrdinal))
        getBase
          | isNothing (structBase decl) =
            FunBind
              [Match noLoc (Ident "bondGetBase") [PWildCard] Nothing
                 (UnGuardedRhs $
                    App (Var $ unqual "error") (Lit $ String "no base struct"))
                 (BDecls [])]
          | otherwise =
            PatBind noLoc (PVar $ Ident "bondGetBase")
              (UnGuardedRhs $ Var $ UnQual baseStructField)
              (BDecls [])
        setBase
          | isNothing (structBase decl) =
            Match noLoc (Ident "bondSetBase") [PWildCard, PWildCard] Nothing
              (UnGuardedRhs $
                 App (Var $ unqual "error") (Lit $ String "no base struct"))
              (BDecls [])
          | otherwise =
            Match noLoc (Ident "bondSetBase") [PVar recVar, PVar baseVar]
              Nothing
              (UnGuardedRhs $
                 RecUpdate (Var $ UnQual recVar)
                   [FieldUpdate (UnQual baseStructField) (Var $ UnQual baseVar)])
              (BDecls [])
        makeSetFunc funcname
          Field{fieldOrdinal, fieldName, fieldType = BT_TypeParam t}
          = Match noLoc funcname
              [PParen
                 (PApp (qualInt "Ordinal")
                    [PLit Signless (Int $ fromIntegral fieldOrdinal)]),
               PVar typeVar]
              Nothing
              (GuardedRhss
                 [GuardedRhs noLoc
                    [Qualifier
                       (InfixApp (Var $ UnQual typeVar) (QVarOp $ UnQual $ Symbol "==")
                          (App (Var $ qualInt "getWireType")
                             (Paren $
                                ExpTypeSig noLoc (Con $ qualInt "Proxy")
                                  (TyApp (TyCon $ qualInt "Proxy")
                                     (TyVar $ mkVar $ paramName t)))))]
                    (InfixApp
                       (InfixApp (Var $ unqual "return") (QVarOp $ UnQual $ Symbol ".")
                          (App (Var $ unqual "fmap")
                             (Paren $
                                Lambda noLoc [PVar valueVar, PVar recVar]
                                  (RecUpdate (Var $ UnQual recVar)
                                     [FieldUpdate (UnQual $ mkVar fieldName)
                                        (Var $ UnQual valueVar)]))))
                       (QVarOp $ UnQual $ Symbol "=<<")
                       (Var $ qualInt "bondGet"))])
              (BDecls [])
        makeSetFunc funcname
          Field{fieldOrdinal, fieldName, fieldType,
                fieldDefault = Just DefaultNothing}
          = Match noLoc funcname
              [PParen
                 (PApp (qualInt "Ordinal")
                    [PLit Signless (Int $ fromIntegral fieldOrdinal)]),
               PApp (wireType fieldType) []]
              Nothing
              (UnGuardedRhs $
                 InfixApp
                   (InfixApp (Var $ unqual "return") (QVarOp $ UnQual $ Symbol ".")
                      (App (Var $ unqual "fmap")
                         (Paren $
                            Lambda noLoc [PVar valueVar, PVar recVar]
                              (RecUpdate (Var $ UnQual recVar)
                                 [FieldUpdate (UnQual $ mkVar fieldName)
                                    (App (Con $ unqual "Just") (Var $ UnQual valueVar))]))))
                   (QVarOp $ UnQual $ Symbol "=<<")
                   (Var $ qualInt "bondGet"))
              (BDecls [])
        makeSetFunc funcname Field{fieldOrdinal, fieldName, fieldType}
          = Match noLoc funcname
              [PParen
                 (PApp (qualInt "Ordinal")
                    [PLit Signless (Int $ fromIntegral fieldOrdinal)]),
               PApp (wireType fieldType) []]
              Nothing
              (UnGuardedRhs $
                 InfixApp
                   (InfixApp (Var $ unqual "return") (QVarOp $ UnQual $ Symbol ".")
                      (App (Var $ unqual "fmap")
                         (Paren $
                            Lambda noLoc [PVar valueVar, PVar recVar]
                              (RecUpdate (Var $ UnQual recVar)
                                 [FieldUpdate (UnQual $ mkVar fieldName)
                                    (Var $ UnQual valueVar)]))))
                   (QVarOp $ UnQual $ Symbol "=<<")
                   (Var $ qualInt "bondGet"))
              (BDecls [])
        defaultSetFunc funcname
          = Match noLoc funcname [PWildCard, PWildCard] Nothing
              (UnGuardedRhs $
                 App (Var $ unqual "return") (Con $ unqual "Nothing"))
              (BDecls [])
        checkField
          = FunBind $
              map makeCheckFunc (structFields decl) ++ [defaultCheckFunc]
        makeCheckFunc Field{fieldOrdinal, fieldName}
          = Match noLoc (Ident "bondFieldHasDefaultValue")
              [PVar recVar,
               PParen $
                 PApp (qualInt "Ordinal")
                   [PLit Signless (Int $ fromIntegral fieldOrdinal)]]
              Nothing
              (UnGuardedRhs $
                 InfixApp
                   (App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar))
                   (QVarOp $ qualInt "equalToDefault")
                   (App (Var $ UnQual $ mkVar fieldName)
                      (Paren $
                         ExpTypeSig noLoc (Var $ qualInt "defaultValue")
                           (makeType False (Ident $ convertTypeName (declName decl))
                              (declParams decl)))))
              (BDecls [])
        defaultCheckFunc
          = Match noLoc (Ident "bondFieldHasDefaultValue")
              [PWildCard, PWildCard]
              Nothing
              (UnGuardedRhs $
                 App (Var $ unqual "error") (Lit $ String "unknown ordinal"))
              (BDecls [])
        putField
          = FunBind $ map makePutFunc (structFields decl) ++ [defaultPutFunc]
        makePutFunc
          Field{fieldOrdinal, fieldName, fieldDefault = Just DefaultNothing}
          = Match noLoc (Ident "bondPutField")
              [PVar recVar,
               PParen $
                 PApp (qualInt "Ordinal")
                   [PLit Signless (Int $ fromIntegral fieldOrdinal)]]
              Nothing
              (UnGuardedRhs $
                 App (Var $ qualInt "bondPutMaybe")
                   (Paren $
                      App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar)))
              (BDecls [])
        makePutFunc Field{fieldOrdinal, fieldName}
          = Match noLoc (Ident "bondPutField")
              [PVar recVar,
               PParen $
                 PApp (qualInt "Ordinal")
                   [PLit Signless (Int $ fromIntegral fieldOrdinal)]]
              Nothing
              (UnGuardedRhs $
                 App (Var $ qualInt "bondPut")
                   (Paren $
                      App (Var $ UnQual $ mkVar fieldName) (Var $ UnQual recVar)))
              (BDecls [])
        defaultPutFunc
          = Match noLoc (Ident "bondPutField") [PWildCard, PWildCard] Nothing
              (UnGuardedRhs $
                 App (Var $ unqual "error") (Lit $ String "unknown ordinal"))
              (BDecls [])
bondStructInstance _ _ = error "bondStructInstance not implemented"