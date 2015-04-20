-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Tests.Util
    ( stringDiff
    ) where

import Data.Algorithm.Diff

stringDiff :: String -> String -> String
stringDiff expected actual =
    unlines $ map lineDiff $ getDiff (lines expected) (lines actual)
  where
    lineDiff (Both s _) = " " ++ s
    lineDiff (First s) = "+" ++ s
    lineDiff (Second s) = "-" ++ s

