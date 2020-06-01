module WorkSpaceSpec (spec) where

import Test.Hspec
import qualified WorkSpace as WS

spec :: Spec
spec = do
    describe "emptyWorkspace"
        it "Creates a WorkSpace with zero entities" $
            (length . WS.entities) WS.emptyWorkspace `shouldBe` 0
