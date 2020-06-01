module WorkSpaceSpec (spec) where

import Test.Hspec
import qualified WorkSpace as WS

spec :: Spec
spec = do
    describe "emptyWorkspace" $ do
        it "Creates a WorkSpace with zero entities" $
            (entityCount WS.emptyWorkSpace) `shouldBe` 0

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================

entityCount :: WS.WorkSpace -> Int
entityCount ws = vs + es
    where vs = (length . WS.getVertices) ws
          es = (length . WS.getEdges) ws
