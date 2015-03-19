import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Tests.Schema

tests :: [Test]
tests =
    [ testGroup "AST"
        [ testProperty "roundtripAST" roundtripAST
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
