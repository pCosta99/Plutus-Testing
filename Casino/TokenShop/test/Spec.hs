module Main
    ( main
    ) where

import qualified Spec.ModelWithClose
import qualified Spec.TraceWithClose
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.TraceWithClose.tests
    --, Spec.ModelWithClose.tests
    ]
