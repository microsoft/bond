-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.Codegen.Util
    (
      utilTestGroup
    ) where

import Language.Bond.Codegen.Util(uniqueName)
import Test.Tasty
import Test.Tasty.QuickCheck

utilTestGroup = testGroup "Codegen Utils"
  [ testGroup "uniqueName" [ testProperty "unique when taken" prop_collisionReturnsNotSame,
                             testProperty "given when not taken" prop_noCollisionReturnsSame]]

prop_collisionReturnsNotSame xs = not (null xs) ==> uniqueName (head xs) xs /= head xs
prop_noCollisionReturnsSame xs = not ("some" `elem` xs) ==> uniqueName "some" xs == "some"
