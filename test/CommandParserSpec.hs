module CommandParserSpec where

import Test.Hspec
import Models
import CommandParser

spec :: Spec
spec = do
    describe "Commands" $ do
        it "exit" $ do
            parseCommand "exit" `shouldBe` Just Exit
        it "help" $ do
            parseCommand "help" `shouldBe` Just Help
    describe "Actions" $ do
        it "add" $ do
            parseCommand "add Test 1" `shouldBe` Just (TodoAction (Add "Test 1"))
        it "remove" $ do
            parseCommand "remove 2" `shouldBe` Just (TodoAction (Remove 2))
        it "remove InvalidIndex" $ do
            parseCommand "remove invalid" `shouldBe` Nothing
        it "complite" $ do
            parseCommand "complite 3" `shouldBe` Just (TodoAction $ Complite 3)
        it "complite InvalidIndex" $ do
            parseCommand "complite invalid" `shouldBe` Nothing
        it "uncomplite" $ do
            parseCommand "uncomplite 3" `shouldBe` Just (TodoAction $ Uncomplite 3)
        it "uncomplite InvalidIndex" $ do
            parseCommand "uncomplite invalid" `shouldBe` Nothing