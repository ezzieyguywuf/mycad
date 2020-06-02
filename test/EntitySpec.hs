module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E

nullE :: E.Entity Int
nullE = E.nullEntity

spec :: Spec
spec = do
    describe "emptyWorkspace" $ do
        it "Creates an Entity with zero sub-components" $
            (subCount nullE) `shouldBe` 0
    describe "addVertex" $ do
        it "Adds a single Vertex to the Entity" $
            (getDelta E.addVertex E.getVertices nullE) `shouldBe` 0
        it "Creates a Vertex at the given Geometry" $
            let (x,y,z) = (10, 20, 0)
                e     = E.addVertex nullE x y z
                v     = E.getVertex e -1
            E.getComponents v `shouldBe` (x, y, z)

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type EntityMod a = E.Entity a -> E.Entity a
type GetComponents a b = E.Entity a -> [b]

subCount :: E.Entity a -> Int
subCount ws = vs + es
    where vs = (length . E.getVertices) ws
          es = (length . E.getEdges) ws

getDelta :: EntityMod a -> GetComponents a b -> E.Entity a -> Int
getDelta f g e = xs' - xs
    where xs  = (length . g) e
          xs' = (length . g) $ f e
