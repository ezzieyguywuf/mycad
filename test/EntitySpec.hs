module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E

spec :: Spec
spec = do
    describe "emptyWorkspace" $ do
        it "Creates a WorkSpace with zero entities" $
            (subCount E.nullEntity) `shouldBe` 0

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================

subCount :: E.Entity a -> Int
subCount ws = vs + es
    where vs = (length . E.getVertices) ws
          es = (length . E.getEdges) ws
