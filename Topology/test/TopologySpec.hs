module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T
import Control.Monad.State
import Data.Maybe (fromJust, isNothing)

spec :: Spec
spec = do
    let makeRayEdge' e = fmap fromJust (T.makeRayEdge e)
    describe "addFreeVertex" $ do
        it "Is inversed by removeVertex, resulting in original state" $
            property (prop_addRemoveIdentity (T.addFreeVertex >>= T.removeVertex))
        it "Creates one new Vertex" $
            property (prop_addsOneVertex T.addFreeVertex)
        it "Does not modify the Edges" $
            property (prop_doesNotModifyEdges T.addFreeVertex)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeVertex)
    describe "addFreeEdge" $ do
        it "Is inversed by removeEdge, resulting in the original state" $
            property (prop_addRemoveIdentity (T.addFreeEdge >>= T.removeEdge))
        it "Does not modify the Vertices" $
            property (prop_doesNotModifyVertices T.addFreeEdge)
        it "Adds a single new Edge" $
            property (prop_addsOneEdge T.addFreeEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeEdge)
    describe "makeRayEdge" $ do
        let prep  = T.addFreeEdge
            run = makeRayEdge' >=> T.removeVertex
            run'  = T.addFreeEdge >>= makeRayEdge'
        it "Is inversed by removeVertex, resulting in the original state" $
            property (prop_addRemoveIdentity' prep run)
        it "Adds one vertex" $
            property (prop_addsOneVertex run')
        it "Makes the Edge adjacent to the added Vertex" $
            property (prop_addAdjacencyToEdge prep makeRayEdge')
        it "Does not add or subtract any Edges" $
            property (prop_doesNotAddOrRemoveEdges prep T.makeRayEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces run')
        context "an Edge that is not a 'Free' Edge is provided'" $ do
            let prep' = prep >>= makeRayEdge' >>= vertToAdjEdge
                rayRun = T.makeRayEdge
            it "returns Nothing" $
                property (prop_returnsNothing $ prep' >>= rayRun)
            it "does not modify the Vertices" $
                property (prop_doesNotModifyVertices' prep' rayRun)
            it "does not modify the Edges" $
                property (prop_doesNotModifyEdges' prep' rayRun)
            it "does not modify the Faces" $
                property (prop_doesNotModifyFaces' prep' rayRun)
        context "an invalid Edge is provided" $ do
            let prep' = do
                    t <- get
                    a <-prep
                    put t
                    pure a
                rayRun = T.makeRayEdge
            it "returns Nothing" $
                property (prop_returnsNothing $ prep' >>= rayRun)
            it "does not modify the Vertices" $
                property (prop_doesNotModifyVertices $ prep' >>= rayRun)
            it "does not modify the Edges" $
                property (prop_doesNotModifyEdges $ prep' >>= rayRun)
            it "does not modify the Faces" $
                property (prop_doesNotModifyFaces $ prep' >>= rayRun)
    describe "closeRayEdge" $ do
        let prep = T.addFreeEdge >>= makeRayEdge' >>= vertToAdjEdge
            run  = T.closeRayEdge
            remove' = T.removeVertex . fromJust
            run' e = fmap fromJust (T.closeRayEdge e)
        it "Is inversed by removeVertex, resulting in the orignal state" $
            property (prop_addRemoveIdentity'  prep (run >=> remove'))
        it "Adds one vertex" $
            property (prop_addsOneVertex' prep run)
        it "Makes the Edge adjacent to the added Vertex" $
            property (prop_addAdjacencyToEdge prep run')
        it "does not modify the Faces" $
            property (prop_doesNotModifyFaces $ prep >>= run)
    describe "addRayEdge" $ do
        let prep   = T.addFreeVertex
            run    = T.addRayEdge
            run'   = T.addRayEdge >=> (pure . fromJust)
            remove = T.removeEdge
        it "Is inversed by removeEdge, resulting in the orignal state" $
            property (prop_addRemoveIdentity'  prep (run >=> (remove . fromJust)))
        it "Does not modify the Vertices" $
            property (prop_doesNotModifyVertices' prep run)
        it "Adds a single Edge" $
            property (prop_addsOneEdge (prep >>= run))
        it "Makes the Vertex adjacent to the added Edge" $
            property (prop_addAdjacencyToVertex prep run')
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces' prep run)
        context "an invalid Edge is provided" $ do
            let prep' = do
                    t <- get
                    a <-prep
                    put t
                    pure a
            it "returns Nothing" $
                property (prop_returnsNothing $ prep' >>= run)
            it "does not modify the Vertices" $
                property (prop_doesNotModifyVertices $ prep' >>= run)
            it "does not modify the Edges" $
                property (prop_doesNotModifyEdges $ prep' >>= run)
            it "does not modify the Faces" $
                property (prop_doesNotModifyFaces $ prep' >>= run)
-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_returnsNothing :: T.TopoState (Maybe a) -> T.Topology -> Bool
prop_returnsNothing s t = evalState test t
    where test = do
            m <- s
            pure $ isNothing m

prop_addRemoveIdentity :: T.TopoState a -> T.Topology -> Bool
prop_addRemoveIdentity run t = evalState test t
    where test = do
            s  <- get
            s' <- run >> get
            pure $ s == s'

prop_addRemoveIdentity' :: T.TopoState a -> TopoMod a b-> T.Topology -> Bool
prop_addRemoveIdentity' prep run t = evalState test t
    where test = do
            a <- prep
            s <- get
            s'<- (run a) >> get
            pure $ s == s'

prop_addsOneVertex :: T.TopoState a -> T.Topology -> Bool
prop_addsOneVertex = deltaXIsN T.getVertices 1

prop_addsOneVertex' :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prop_addsOneVertex' = prep_deltaXIsN T.getVertices 1

prop_addsOneEdge :: T.TopoState a -> T.Topology -> Bool
prop_addsOneEdge = deltaXIsN T.getEdges 1

prop_doesNotAddOrRemoveEdges :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prop_doesNotAddOrRemoveEdges = prep_deltaXIsN T.getEdges 0

prop_doesNotModifyVertices :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyVertices = doesNotModifyX T.getVertices

prop_doesNotModifyVertices' :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prop_doesNotModifyVertices' = prep_doesNotModifyX T.getVertices

prop_doesNotModifyEdges' :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prop_doesNotModifyEdges' = prep_doesNotModifyX T.getEdges

prop_doesNotModifyFaces' :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prop_doesNotModifyFaces' = prep_doesNotModifyX T.getFaces

prop_doesNotModifyEdges :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyEdges = doesNotModifyX T.getEdges

prop_doesNotModifyFaces :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyFaces = doesNotModifyX T.getFaces

_prop_doesNotAddOrDeleteEdges :: T.TopoState T.Edge -> (T.Edge -> T.TopoState a) -> T.Topology -> Bool
_prop_doesNotAddOrDeleteEdges prep run t0 = evalState test t0
    where test = do
            t <- prep
            es <- (length . T.getEdges) <$> get
            run t
            es' <- (length . T.getEdges) <$> get
            pure (es == es')

prop_addAdjacencyToEdge :: T.TopoState T.Edge -> TopoMod T.Edge T.Vertex -> T.Topology -> Bool
prop_addAdjacencyToEdge prep run initial = evalState test initial
    where test = do
            e <- prep
            v <- run e
            t <- get
            pure $ elem v $ T.edgeAdjacentVertices t e

prop_addAdjacencyToVertex :: T.TopoState T.Vertex -> TopoMod T.Vertex T.Edge -> T.Topology -> Bool
prop_addAdjacencyToVertex prep run initial = evalState test initial
    where test = do
            v <- prep
            e <- run v
            t <- get
            pure $ elem e $ T.vertexAdjacentEdges t v

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type TopoGetter a = T.Topology -> [a]
type TopoMod a b = a -> T.TopoState b
deltaXIsN :: TopoGetter b -> Int -> T.TopoState a -> T.Topology -> Bool
deltaXIsN getter n run initial = prep_deltaXIsN getter n noPrep run' initial
    where noPrep = pure id
          run'  = \_ -> run

prep_deltaXIsN :: TopoGetter a -> Int -> T.TopoState b -> TopoMod b c -> T.Topology -> Bool
prep_deltaXIsN getter n prep run initial = evalState test initial
    where test = do
            b <- prep
            xs <- gets (length . getter)
            xs' <- run b >> gets (length . getter)
            pure (xs' - xs == n)

doesNotModifyX :: Eq a => TopoGetter a -> T.TopoState b -> T.Topology -> Bool
doesNotModifyX getter run initial = evalState test initial
    where test = do
            xs <- gets getter
            xs' <- run >> gets getter
            pure (xs' == xs)

prep_doesNotModifyX :: Eq c => TopoGetter c -> T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prep_doesNotModifyX getter prep run initial = evalState test initial
    where test = do
            a <- prep
            xs  <- gets getter
            xs' <- run a >> gets getter
            pure (xs == xs')

vertToAdjEdge :: T.Vertex -> T.TopoState T.Edge
vertToAdjEdge v = do
    t <- get
    pure . head $ T.vertexAdjacentEdges t v
