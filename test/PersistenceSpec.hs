module PersistenceSpec where

import Test.Hspec
import Models
import Persistence

spec :: Spec
spec = do
    describe "todosToStr" $ do
        it "basic case" $ do
            let todos = [ TodoItem True "Test 1"
                        , TodoItem False "Test 2"
                        , TodoItem False "Test 3" ]
            let expected = "[x] Test 1\n" ++ "[ ] Test 2\n" ++ "[ ] Test 3"
            todosToStr todos `shouldBe` expected
        it "empty list" $ do
            let todos = []
            let expected = ""
            todosToStr todos `shouldBe` expected
    describe "parseTodos" $ do
        it "basic case" $ do
            let input = "[x] Test 1\n" ++ "[ ] Test 2\n" ++ "[ ] Test 3"
            let expected = Just [ TodoItem True "Test 1"
                                , TodoItem False "Test 2"
                                , TodoItem False "Test 3" ]
            parseTodos input `shouldBe` expected
        it "empty list" $ do
            let input = ""
            let expected = Just []
            parseTodos input `shouldBe` expected
        it "invalid input" $ do
            let input = "Invalid input"
            let expected = Nothing
            parseTodos input `shouldBe` expected

