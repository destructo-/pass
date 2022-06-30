module Domain.ResourceSpec (spec) where

import Test.Hspec (
    shouldBe
  , parallel
  , it
  , describe
  , Spec
  , Expectation
  , shouldReturn
  , shouldContain, shouldNotContain)

import qualified Config
import Domain.Resource (Resource(..), makeFullDirectory, makeFullPath)


spec :: Spec
spec = parallel $ do
    describe "when make path to directory from relative path in Config" $ do
        it "must crete full path like /home/{USER}/{PATH_TO_DIR_FROM_CONF}" $ do
            path <- makeFullDirectory Config.dataResource
            path `shouldContain` relativeDir Config.dataResource
            path `shouldNotContain` fileName Config.dataResource

    describe "when make path to file from relative path in Config" $ do
        it "must create fill path like /home/{USER}/{PATH_TO_DIR_FROM_CONF}/{FILE_NAME_FROM_CONF}" $ do
            path <- makeFullPath Config.dataResource
            path `shouldContain` relativeDir Config.dataResource
            path `shouldContain` fileName Config.dataResource