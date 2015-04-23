-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE TemplateHaskell #-}

module Tests.Schema
    ( roundtripAST
    , compareAST
    ) where

import System.Exit
import System.IO
import System.Directory
import Data.Aeson (encode, decode, eitherDecode)
import Data.DeriveTH
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import Test.QuickCheck
import Test.HUnit
import Bond.Schema.Types
import Bond.Schema.JSON
import Bond.Parser
import Files

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

