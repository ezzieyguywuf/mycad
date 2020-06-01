module WorkSpaceSpec (spec) where

import Test.Hspec
import qualified WorkSpace as WS

spec :: Spec
spec = do
    describe "emptyWorkspace" $ do
        it "Creates a WorkSpace with zero entities" $
            (WS.entityCount WS.emptyWorkSpace) `shouldBe` 0
