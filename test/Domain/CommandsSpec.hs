module Domain.CommandsSpec (spec) where

import Test.Hspec (
    context
  , describe
  , it
  , parallel
  , shouldBe
  , Spec )

import Domain.Commands(Command(..), fromMaybeString)

spec :: Spec
spec = parallel $ do
    describe "Commands: create commands for arguments start from '-'" $ do
        context "when crete command from key doesn't starts from '-'" $ do
            it "must return Find" $ do
                let argument = Just "word"
                fromMaybeString argument `shouldBe` Find

        context "when create command without key" $ do
            it "must return help command" $do
                let argument = Nothing
                fromMaybeString argument `shouldBe` Help

        context "when create command from unsupported argument" $ do
            it "must return help" $ do
                let argument = Just "-badArg"
                fromMaybeString argument `shouldBe` Help

        context "when crete command from correct argument" $ do
            it "must return corresponding command" $ do
                let add = Just "-add"
                let del = Just "-del"
                let lst = Just "-list"
                let hlp = Just "-help"
                fromMaybeString add `shouldBe` Add
                fromMaybeString del `shouldBe` Delete
                fromMaybeString lst `shouldBe` List
                fromMaybeString hlp `shouldBe` Help
