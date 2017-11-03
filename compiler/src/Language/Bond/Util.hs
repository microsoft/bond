-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : provisional
Portability : portable

Helper functions for combining elements into common constructs. These functions
can be used in code generation to lazily combine 'Text' elements but they are
more generic and work for any 'Monoid'.
-}

module Language.Bond.Util
    ( sepBy
    , sepEndBy
    , sepBeginBy
    , optional
    , ifThenElse
    , angles
    , brackets
    , braces
    , parens
    , between
    ) where

import Data.Monoid
import Data.String (IsString)
import Prelude

sepEndBy, sepBeginBy, sepBy :: (Monoid a, Eq a) => a -> (b -> a) -> [b] -> a

-- | Maps elements of a list and combines them with 'mappend' using given
-- separator, ending with a separator.
sepEndBy _ _ [] = mempty
sepEndBy s f (x:xs)
    | next == mempty = rest
    | otherwise = next <> s <> rest
        where
            next = f x
            rest = sepEndBy s f xs

-- | Maps elements of a list and combines them with 'mappend' using given
-- separator, starting with a separator.
sepBeginBy _ _ [] = mempty
sepBeginBy s f (x:xs)
    | next == mempty = rest
    | otherwise = s <> next <> rest
    where
        next = f x
        rest = sepBeginBy s f xs

-- | Maps elements of a list and combines them with 'mappend' using given
-- separator.
sepBy _ _ [] = mempty
sepBy s f (x:xs)
    | null xs = next
    | next == mempty = rest
    | otherwise = next <> sepBeginBy s f xs
        where
            next = f x
            rest = sepBy s f xs

-- | The function takes a function and a Maybe value. If the Maybe value is
-- Nothing, the function returns 'mempty', otherwise, it applies the function
-- to the value inside 'Just' and returns the result.
optional :: (Monoid m) => (a -> m) -> Maybe a -> m
optional = maybe mempty

-- if-then-else as a function
ifThenElse :: Bool -> a -> a -> a
ifThenElse True thenCondition _ = thenCondition
ifThenElse False _ elseCondition = elseCondition

-- | If the 3rd argument is not 'mempty' the function wraps it between the
-- first and second argument using 'mappend', otherwise it return 'mempty'.
between :: (Monoid a, Eq a) => a -> a -> a -> a
between l r m
    | m == mempty = mempty
    | otherwise = l <> m <> r

angles, brackets, braces, parens :: (Monoid a, IsString a, Eq a) => a -> a
-- | Wraps the string argument between @<@ and @>@, unless the argument is
-- 'mempty' in which case the function returns 'mempty'.
angles m = between "<" ">" m

-- | Wraps the string argument between @[@ and @]@, unless the argument is
-- 'mempty' in which case the function returns 'mempty'.
brackets m = between "[" "]" m

-- | Wraps the string argument between @{@ and @}@, unless the argument is
-- 'mempty' in which case the function returns 'mempty'.
braces m = between "{" "}" m

-- | Wraps the string argument between @(@ and @)@, unless the argument is
-- 'mempty' in which case the function returns 'mempty'.
parens m = between "(" ")" m

