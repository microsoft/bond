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
    bondInput <- readFileUtf8 $ "tests" </> file <.> "bond"
    jsonInput <- BL.readFile $ "tests" </> file <.> "json"
    bondParsed <- parseBond "" bondInput "" noImports
    let jsonParsed = eitherDecode jsonInput
    case bondParsed of
        Left error -> do
            print $ "Error parsing " ++ file ++ ".bond: " ++ show error
            exitFailure
        Right bond -> 
            case jsonParsed of
                Left error -> do
                    print $ "Error parsing " ++ file ++ ".json: " ++ show error
                    exitFailure
                Right json ->
                    assertEqual "" bond json
  where
    noImports _ _ = return ("", "")

readFileUtf8 :: FilePath -> IO String
readFileUtf8 name = do 
    h <- openFile name ReadMode
    hSetEncoding h utf8_bom
    hGetContents h
