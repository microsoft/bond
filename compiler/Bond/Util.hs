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

sepEndBy s f [] = mempty
sepEndBy s f (x:xs) 
    | next == mempty = rest
    | otherwise = next <> s <> rest
        where
            next = f x
            rest = sepEndBy s f xs

sepBeginBy s f [] = mempty
sepBeginBy s f (x:xs)
    | next == mempty = rest 
    | otherwise = s <> next <> rest
    where
        next = f x
        rest = sepBeginBy s f xs

sepBy s f [] = mempty
sepBy s f (x:xs)
    | null xs = next
    | next == mempty = rest
    | otherwise = next <> sepBeginBy s f xs
        where
            next = f x
            rest = sepBy s f xs

optional :: (Monoid m) => (a -> m) -> Maybe a -> m
optional = maybe mempty

between l r m
    | m == mempty = mempty
    | otherwise = l <> m <> r

angles m = between "<" ">" m
brackets m = between "[" "]" m
braces m = between "{" "}" m
parens m = between "(" ")" m

