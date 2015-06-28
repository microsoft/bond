-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Syntax
    ( roundtripAST
    , compareAST
    ) where

import Data.Aeson (encode, decode)
import Data.DeriveTH
import System.FilePath
import Test.QuickCheck
import Test.HUnit
import Language.Bond.Syntax.Types
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

