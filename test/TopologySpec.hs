module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T
import Control.Monad.State

spec :: Spec
spec = do
    describe "addFreeVertex" $ do
        it "Is inversed by removeVertex, resulting in original state" $
            property (prop_addRemoveIdentity (T.addFreeVertex >>= T.removeVertex))
        it "Creates one new Vertex" $
            property (prop_addsOneVertex T.addFreeVertex)
        it "Does not modify the Edges" $
            property (prop_doesNotModifyEdges T.addFreeVertex)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeVertex)
    describe "addEdge" $ do
        it "Is inversed by removeEdge, resulting in the original state" $
            property (prop_addRemoveIdentity (T.addEdge >>= T.removeEdge))
        it "Adds two vertices" $
            property (prop_addsTwoVertices T.addEdge)
        it "Creates one new Edge" $
            property (prop_addsOneEdge T.addEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addEdge)
        it "Creates an Edge with two adjacent Vertices" $
            -- Since we know that:
            --
            --     1. It only adds two vertices
            --     2. It only adds one Edge
            --     3. Does not modify Faces
            --     4. It can be inversed using removeEdge - taking with the three above,
            --        this means that the function did not modify any of the existing
            --        Topology. Otherwise, the remove operation would result in the same
            --        number of topological entities as before, but a different topology.
            --
            -- Then it is logical to infer that the Edge with two adjacent Vertices *must*
            -- be the same as the Edge which was added.
            property (prop_EdgeHasTwoAdjacentVertices T.addEdge)
    describe "addEdgeToVertex" $ do
        it "Adds a single Vertex" $
            property (prop_addsOneVertex' T.addFreeVertex T.addEdgeToVertex)
        it "Adds a single Edge" $
            -- Note, we've already tested that addFreeVertex does not modify Edge
            property (prop_addsOneEdge (T.addFreeVertex >>= T.addEdgeToVertex))
        it "Does not modify Faces" $
            -- Note, we've already tested that addFreeVertex does not modify Face
            property (prop_doesNotModifyFaces (T.addFreeVertex >>= T.addEdgeToVertex))
        it "Creates an Edge with two adjacent Vertices" $
            property (prop_EdgeHasTwoAdjacentVertices' (T.addFreeVertex >>= T.addEdgeToVertex))

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_addRemoveIdentity :: T.TopoState a -> T.Topology -> Bool
prop_addRemoveIdentity run t = evalState test t
    where test = do
            s  <- get
            s' <- run >> get
            pure $ s == s'

prop_addsOneVertex :: T.TopoState a -> T.Topology -> Bool
prop_addsOneVertex = deltaXIsN T.getVertices 1

prop_addsOneVertex' :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
prop_addsOneVertex' = prep_deltaXIsN T.getVertices 1

prop_addsTwoVertices :: T.TopoState a -> T.Topology -> Bool
prop_addsTwoVertices = deltaXIsN T.getVertices 2

prop_addsOneEdge :: T.TopoState a -> T.Topology -> Bool
prop_addsOneEdge = deltaXIsN T.getEdges 1

_prop_doesNotModifyVertices :: T.TopoState a -> T.Topology -> Bool
_prop_doesNotModifyVertices = deltaXIsN T.getVertices 0

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

prop_EdgeHasTwoAdjacentVertices :: T.TopoState T.Edge -> T.Topology -> Bool
prop_EdgeHasTwoAdjacentVertices s t = evalState test t
    where test = do
            e <- s
            vs <- gets (flip T.adjVertToEdge e)
            pure ((length vs) == 2)

prop_EdgeHasTwoAdjacentVertices' :: T.TopoState (Maybe T.Edge) -> T.Topology -> Bool
prop_EdgeHasTwoAdjacentVertices' s t = evalState test t
    where test = do
            m <- s
            case m of
                Just e -> do vs <- gets (flip T.adjVertToEdge e)
                             pure ((length vs) == 2)
                Nothing -> pure False

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
prep_deltaXIsN getter n prep mod initial = evalState test initial
    where test = do
            b <- prep
            xs <- gets (length . getter)
            mod b
            xs' <- gets (length . getter)
            pure (xs' - xs == n)

doesNotModifyX :: Eq a => TopoGetter a -> T.TopoState b -> T.Topology -> Bool
doesNotModifyX getter run initial = evalState test initial
    where test = do
            xs <- gets getter
            xs' <- run >> gets getter
            pure (xs' == xs)
