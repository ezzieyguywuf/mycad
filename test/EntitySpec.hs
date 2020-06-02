module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E
import qualified Geometry as Geo

nullE :: E.Entity Float
nullE = E.nullEntity

spec :: Spec
spec = do
    describe "nullEntity" $ do
        it "Creates an Entity with a single Vertex" $
            (length . E.getVertices $ nullE) `shouldBe` 1
        it "the Vertex is at the 'Origin'" $
            let p = Geo.makePoint 0 0 0
                v = last . E.getVertices $ nullE
            in (E.getPoint v) `shouldBe` p
        it "Creates an Entity with zero Edges" $
            (length . E.getEdges $ nullE) `shouldBe` 0
