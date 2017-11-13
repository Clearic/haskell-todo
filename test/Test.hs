module Main where

import Test.Hspec
import qualified ReducerSpec
import qualified CommandParserSpec
import qualified ModelsSpec
import qualified PersistenceSpec

main :: IO ()
main = hspec $ do
    describe "Reducer" ReducerSpec.spec
    describe "CommandParser" CommandParserSpec.spec
    describe "Models" ModelsSpec.spec
    describe "PersistenceSpec" PersistenceSpec.spec
