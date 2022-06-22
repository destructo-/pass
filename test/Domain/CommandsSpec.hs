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
    describe "when crete command from key doesn't starts from '-'" $ do
        it "must return Find command" $ do
            let argument = Just "word"
            fromMaybeString argument `shouldBe` Find

    describe "when create command without key" $ do
        it "must return Help command" $do
            let argument = Nothing
            fromMaybeString argument `shouldBe` Help

    describe "when create command from unsupported argument" $ do
        it "must return Help command" $ do
            let argument = Just "-badArg"
            fromMaybeString argument `shouldBe` Help

    describe "when crete command from correct argument" $ do
        it "must return corresponding command" $ do
            let add = Just "-add"
            let del = Just "-del"
            let upd = Just "-upd"
            let lst = Just "-lst"
            let hlp = Just "-help"
            fromMaybeString add `shouldBe` Add
            fromMaybeString del `shouldBe` Delete
            fromMaybeString lst `shouldBe` List
            fromMaybeString hlp `shouldBe` Help
            fromMaybeString upd `shouldBe` Update
