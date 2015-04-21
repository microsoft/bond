-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings #-}

module Bond.Util
    ( sepBy
    , sepEndBy
    , sepBeginBy
    , optional
    , angles
    , brackets
    , braces
    , parens
    , between
    ) where

import Data.Monoid
import Data.String (IsString)
import Prelude

sepEndBy :: (Monoid a, Eq a)
         => a -> (t -> a) -> [t] -> a
sepEndBy _ _ [] = mempty
sepEndBy s f (x:xs) 
    | next == mempty = rest
    | otherwise = next <> s <> rest
        where
            next = f x
            rest = sepEndBy s f xs

sepBeginBy :: (Monoid a, Eq a)
            => a -> (t -> a) -> [t] -> a
sepBeginBy _ _ [] = mempty
sepBeginBy s f (x:xs)
    | next == mempty = rest 
    | otherwise = s <> next <> rest
    where
        next = f x
        rest = sepBeginBy s f xs

sepBy :: (Monoid a, Eq a)
      => a -> (t -> a) -> [t] -> a
sepBy _ _ [] = mempty
sepBy s f (x:xs)
    | null xs = next
    | next == mempty = rest
    | otherwise = next <> sepBeginBy s f xs
        where
            next = f x
            rest = sepBy s f xs

optional :: (Monoid m) => (a -> m) -> Maybe a -> m
optional = maybe mempty

between :: (Monoid a, Eq a) => a -> a -> a -> a
between l r m
    | m == mempty = mempty
    | otherwise = l <> m <> r

angles, brackets, braces, parens
    :: (Monoid a, IsString a, Eq a)
    => a -> a
angles m = between "<" ">" m
brackets m = between "[" "]" m
braces m = between "{" "}" m
parens m = between "(" ")" m

