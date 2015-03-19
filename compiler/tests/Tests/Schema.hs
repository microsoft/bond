{-# LANGUAGE TemplateHaskell #-}

module Tests.Schema
    ( roundtripAST
    ) where

import Data.Aeson (encode, decode)
import Data.DeriveTH
import Test.QuickCheck
import Bond.Schema.Types
import Bond.Schema.JSON

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

