-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Syntax
    ( roundtripAST
    , compareAST
    , aliasResolution
    ) where

import Data.Aeson (encode, decode)
import Data.DeriveTH
import System.FilePath
import Test.QuickCheck
import Test.HUnit
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import IO

derive makeArbitrary ''Attribute
derive makeArbitrary ''Bond
derive makeArbitrary ''Constant
derive makeArbitrary ''Constraint
derive makeArbitrary ''Declaration
derive makeArbitrary ''Default
derive makeArbitrary ''Field
derive makeArbitrary ''Import
derive makeArbitrary ''Language
derive makeArbitrary ''Modifier
derive makeArbitrary ''Namespace
derive makeArbitrary ''Type
derive makeArbitrary ''TypeParam

roundtripAST :: Bond -> Bool
roundtripAST x = (decode . encode) x == Just x

compareAST :: FilePath -> Assertion
compareAST file = do
    bond <- parseBondFile [] $ "tests" </> "schema" </> file <.> "bond"
    json <- parseASTFile $ "tests" </> "schema" </> file <.> "json"
    assertEqual "" bond json

aliasResolution :: Assertion
aliasResolution = do
    let p1 = TypeParam "T" Nothing
    let p2 = TypeParam "U" Nothing
    let a1 = Alias [] "" [] BT_Bool
    let a2 = Alias [] "" [p1] $ BT_TypeParam p1
    let a3 = Alias [] "" [p1] $ BT_List $ BT_TypeParam p1
    let a4 = Alias [] "" [p1, p2] $ BT_Nullable $ BT_Map (BT_TypeParam p2) (BT_Nullable $ BT_TypeParam p1)
    let a5 = Alias [] "" [p1] $ BT_List $ BT_UserDefined a3 [BT_TypeParam p1]
    let a6 = Alias [] "" [p1, p2] $ BT_Vector $ BT_Vector $ BT_TypeParam p2

    assertEqual "" BT_Bool $
        resolveAlias a1 []

    assertEqual "" BT_Int32 $
        resolveAlias a2 [BT_Int32]

    assertEqual "" (BT_Set BT_Int32) $
        resolveAlias a2 [BT_Set BT_Int32]

    assertEqual "" (BT_TypeParam p2) $
        resolveAlias a2 [BT_TypeParam p2]

    assertEqual "" (BT_TypeParam p1) $
        resolveAlias a2 [BT_TypeParam p1]

    assertEqual "" (BT_List BT_Int32) $
        resolveAlias a3 [BT_Int32]

    assertEqual "" (BT_List $ BT_UserDefined a1 []) $
        resolveAlias a3 [BT_UserDefined a1 []]

    assertEqual "" (BT_Nullable $ BT_Map BT_Bool $ BT_Nullable BT_String) $
        resolveAlias a4 [BT_String, BT_Bool]

    assertEqual "" (BT_List $ BT_UserDefined a3 [BT_Set BT_Bool]) $
        resolveAlias a5 [BT_Set BT_Bool]

    assertEqual "" (BT_Vector $ BT_Vector $ BT_List BT_Double) $
        resolveAlias a6 [BT_IntTypeArg 10, BT_List BT_Double]
