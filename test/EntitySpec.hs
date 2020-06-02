module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E
import qualified Geometry as Geo

baseE :: E.Entity Float
baseE = E.baseEntity

spec :: Spec
spec = do
    describe "baseEntity" $ do
        it "Creates an Entity with a single Vertex" $
            (length . E.getVertices $ baseE) `shouldBe` 1
        it "the Vertex is at the 'Origin'" $
            let p = Geo.makePoint 0 0 0
                v = last . E.getVertices $ baseE
            in (E.getPoint v) `shouldBe` p
        it "Creates an Entity with zero Edges" $
            (length . E.getEdges $ baseE) `shouldBe` 0
