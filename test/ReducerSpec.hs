module ReducerSpec where 

import Test.Hspec
import Models
import Reducer

spec :: Spec
spec = do
    describe "Add" $ do
        it "add to empty list" $ do
            reducer [] (Add "Test 1") `shouldBe` Right [(TodoItem False "Test 1")]
        it "list of one" $ do
            let list = [(TodoItem False "Test 1")]
            let actuall = reducer list (Add "Test 2")
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem False "Test 2" ]
            actuall `shouldBe` expected
        it "list of two" $ do
            let list = [ TodoItem False "Test 1"
                       , TodoItem False "Test 2" ]
            let actuall = reducer list (Add "Test 3")
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem False "Test 2"
                                 , TodoItem False "Test 3" ]
            actuall `shouldBe` expected
    describe "remove" $ do
        let list = [ TodoItem False "Test 1"
                   , TodoItem False "Test 2"
                   , TodoItem False "Test 3" ]
        it "first" $ do
            let actuall = reducer list (Remove 1)
            let expected = Right [ TodoItem False "Test 2"
                                 , TodoItem False "Test 3" ]
            actuall `shouldBe` expected
        it "last" $ do
            let actuall = reducer list (Remove 3)
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem False "Test 2" ]
            actuall `shouldBe` expected
        it "middle" $ do
            let actuall = reducer list (Remove 2)
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem False "Test 3" ]
            actuall `shouldBe` expected
        it "out of range" $ do
            let actuall = reducer list (Remove 4)
            actuall `shouldBe` (Left IndexOutOfRange)
        it "negative index" $ do
            let actuall = reducer list (Remove (-1))
            actuall `shouldBe` (Left IndexOutOfRange)
        it "zero index" $ do
            let actuall = reducer list (Remove 0)
            actuall `shouldBe` (Left IndexOutOfRange)
    describe "complite" $ do
        let list = [ TodoItem False "Test 1"
                   , TodoItem False "Test 2"
                   , TodoItem False "Test 3" ]
        it "first" $ do
            let actuall = reducer list (Complite 1)
            let expected = Right [ TodoItem True "Test 1"
                                 , TodoItem False "Test 2"
                                 , TodoItem False "Test 3" ]
            actuall `shouldBe` expected
        it "last" $ do
            let actuall = reducer list (Complite 3)
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem False "Test 2"
                                 , TodoItem True "Test 3" ]
            actuall `shouldBe` expected
        it "middle" $ do
            let actuall = reducer list (Complite 2)
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem True "Test 2"
                                 , TodoItem False "Test 3" ]
            actuall `shouldBe` expected
        it "out of range" $ do
            let actuall = reducer list (Complite 4)
            actuall `shouldBe` (Left IndexOutOfRange)
        it "negative index" $ do
            let actuall = reducer list (Complite (-1))
            actuall `shouldBe` (Left IndexOutOfRange)
        it "zeor index" $ do
            let actuall = reducer list (Complite 0)
            actuall `shouldBe` (Left IndexOutOfRange)
    describe "uncomplite" $ do
        let list = [ TodoItem True "Test 1"
                   , TodoItem True "Test 2"
                   , TodoItem True "Test 3" ]
        it "first" $ do
            let actuall = reducer list (Uncomplite 1)
            let expected = Right [ TodoItem False "Test 1"
                                 , TodoItem True "Test 2"
                                 , TodoItem True "Test 3" ]
            actuall `shouldBe` expected
        it "last" $ do
            let actuall = reducer list (Uncomplite 3)
            let expected = Right [ TodoItem True "Test 1"
                                 , TodoItem True "Test 2"
                                 , TodoItem False "Test 3" ]
            actuall `shouldBe` expected
        it "middle" $ do
            let actuall = reducer list (Uncomplite 2)
            let expected = Right [ TodoItem True "Test 1"
                                 , TodoItem False "Test 2"
                                 , TodoItem True "Test 3" ]
            actuall `shouldBe` expected
        it "out of range" $ do
            let actuall = reducer list (Uncomplite 4)
            actuall `shouldBe` (Left IndexOutOfRange)
        it "negative index" $ do
            let actuall = reducer list (Uncomplite (-1))
            actuall `shouldBe` (Left IndexOutOfRange)
        it "zero index" $ do
            let actuall = reducer list (Uncomplite 0)
            actuall `shouldBe` (Left IndexOutOfRange)
            