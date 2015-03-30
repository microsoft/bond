import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Tests.Schema

tests :: [Test]
tests =
    [ testGroup "AST"
        [ testProperty "roundtrip" roundtripAST
        , testGroup "Compare .bond and .json"
            [ testCase "attributes" $ compareAST "attributes"
            , testCase "basic types" $ compareAST "basic_types"
            , testCase "complex types" $ compareAST "complex_types"
            , testCase "default values" $ compareAST "defaults"
            , testCase "empty" $ compareAST "empty"
            , testCase "field modifiers" $ compareAST "field_modifiers"
            , testCase "generics" $ compareAST "generics"
            , testCase "inheritance" $ compareAST "inheritance"
            , testCase "type aliases" $ compareAST "aliases"
            ]
        ]
    ]

main :: IO ()
main = do
    options <- getArgs >>= interpretArgs
    case options of
        Left error -> do
            putStr error
            exitFailure
        Right (runnerOptions, _) -> do
            let testOptions = maybe mempty id $ ropt_test_options runnerOptions
            let testSize = topt_maximum_test_size testOptions
            defaultMainWithOpts tests $ runnerOptions
                { ropt_test_options = Just testOptions
                    -- Use a smaller maximum test size by default
                    { topt_maximum_test_size = Just $ maybe 15 id testSize
                    }
                }
