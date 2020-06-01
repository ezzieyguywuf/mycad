module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E

spec :: Spec
spec = do
    describe "emptyWorkspace" $ do
        it "Creates an Entity with zero sub-components" $
            (subCount E.nullEntity) `shouldBe` 0
    describe "addVertex" $ do
        it "Adds a single Vertex to the Entity" $
            (getDelta E.addVertex E.getVertices E.nullEntity) `shouldBe` 0

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type EntityMod = E.Entity -> E.Entity
type GetComponents a = E.Entity -> [a]

subCount :: E.Entity a -> Int
subCount ws = vs + es
    where vs = (length . E.getVertices) ws
          es = (length . E.getEdges) ws

getDelta :: EntityMod -> GetComponents a -> E.Entity -> Int
getDelta f g e = xs' - xs
    where xs  = (length . f) e
          xs' = (length . f) $ g e
