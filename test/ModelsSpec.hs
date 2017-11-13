module ModelsSpec where

import Test.Hspec
import Models

spec :: Spec
spec = do
    describe "show TodoItem" $ do
        it "test1" $ do
            show (TodoItem False "Test 1") `shouldBe` "[ ] Test 1"
        it "test2" $ do
            show (TodoItem True "Test 2") `shouldBe` "[x] Test 2"
    describe "red TodoItem" $ do
        it "read1" $ do
            let actuall = readTodoItem "[ ] Test 1"
            let expected = Just $ TodoItem False "Test 1"
            actuall `shouldBe` expected
        it "read2" $ do
            let actuall = readTodoItem "[x] Test 2"
            let expected = Just $ TodoItem True "Test 2"
            actuall `shouldBe` expected
        it "read invalid" $ do
            let actuall = readTodoItem "Invalid input"
            let expected = Nothing
            actuall `shouldBe` expected