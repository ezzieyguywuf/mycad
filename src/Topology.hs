{-|
Module      : Topology
Description : Adjacency relationships for geometric data.
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This module describes a 'Topology' which can be used to manage and introspect the
relationship between geometric entities, specifically Vertices, Edges, and Faces. The
Topology itself is not aware of any geometry - you could use this to model any similar
relationships.

It was written, however, with the intent it be used in a CAD system.
-}
module Topology
( -- * Exported types
  -- | Constructors for these are not exported, therefore you must use the
  --   functions herein to create any of these
  Topology
, TopoState
, Vertex
, Face
, Edge
  -- * Mutating functions
, emptyTopology
, addFreeVertex
, addFreeEdge
, makeRayEdge
, closeRayEdge
, removeVertex
, removeEdge
  -- * Inspection functions
, edgeAdjacentVertices
, vertexAdjacentEdges
, adjVertToEdge
, adjEdgeToVert
, getVertices
, getEdges
, getFaces
  -- * Pretty printing
  -- | In the output, '->' means "out of" and '<-' means "in to". Therefore:
  --
  -- >>> prettyPrintVertex v t
  -- V0: <- None
  --     -> E0
  --
  -- Means that the 'Vertex' @V0@ has __no__ entities ('Edge' or otherwise)
  -- pointing in to it, and a single 'Edge' named @E0@ pointing out of it.
, prettyPrintVertex
, prettyPrintVertices
, prettyPrintEdge
, prettyPrintEdges
, prettyPrintFace
, prettyPrintFaces
, prettyPrintTopology
)where

import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Data.Text.Prettyprint.Doc
import Control.Monad.State as St

-- ===========================================================================
--                               Data Types
-- ===========================================================================
newtype Topology =
    Topology {unTopology :: Gr NodeLabel BridgeLabel}
        deriving (Show, Eq)

type TopoState a = St.State Topology a

newtype Vertex = Vertex Int deriving (Show, Eq)
data Edge = Edge {getEdgeID :: Int}
            deriving (Show, Eq)
newtype Face = Face Int deriving (Show, Eq)

type NodeLabel = (Element, Int)
newtype BridgeLabel = BridgeLabel () deriving (Show, Ord, Eq)
data Element = EVertex | EEdge | EFace deriving (Show, Eq)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
-- TODO notes to self - I want to try to do the "vertex one" instead of the
-- half-edge/winged-edge one. This shouldn't be too hard - I'll have to add anothing value
-- to my Vertex type, maybe call it VertexCluster or something, and rename the Vertex
-- value to PartialVertex.
--
-- Probably also rename the Edge value to HalfEdge.
--
-- Now: any HalfEdge can __only__ connect __from__ a single PartialVertex __to__ a second
-- (or the same) PartialVertex. This can be chained to form a loop - you know you have a
-- loop when you're back at your starting point.
--
-- That's all a HalfEdge is allowed to do!
--
-- The PartialVertex is given a bit more "yum" - it can als link to a VertexCluster. This
-- link is bi-directional.
--
-- Now, any time we give the user a Vertex, we will __only__ give them a VertexCluster
-- (they won't know!! but we do!). This way, any number of PartialVertex can be added to
-- this "cluster" and the user will be none the wiser.
--
-- Now, let's say the user creates a 2-d planar loop. That's nothing but a bunch of
-- HalfEdge and PartialVertex.
--
-- Now, they select one of these "Edge" (notice the quotes) and start to create a new
-- loop. Wellll since WE know that this is actually a HalfEdge we can add a new
-- PartialVertex no the respective VertexCluster, and create a new HalfEdge between these
-- new PartialVertex.
--
-- The link between these twe VertexCluster is now "complete". No more Edges between these
-- two is supported - only one in each direction.
--
-- Thus, for a given pair VertexCluster, the maximum number of PartialVertex needed to
-- identify a full adjacency is 4. Not so bad.
--
-- Finally, we may want to add a RootPartialVertex value to Vertex. This "Root" value can
-- bee used as the starting point for a Loop, and can also (maybe) be used as the "jump
-- off" point to an adjacent Face.

-- | The EmptyTopology is the starting point for any 'Topology'. As no
--   constructors for 'Topology' are exported, this is the only way to create
--   one.
emptyTopology :: Topology
emptyTopology = Topology Graph.empty

-- | Adds a single "free" 'Vertex' to the 'Topology'. In this context, "free"
--   means that it is does not have any entities adjacent to it.
addFreeVertex :: TopoState Vertex
addFreeVertex = do
    n <- addNode EVertex
    pure $ Vertex n

-- | If the Vertex does not exist, this does nothing
--
--   __WARNING__
--   This does __not__ check for any "hanging" adjacencies. In other words, if there was
--   something that
--
--      1. /was/ adjacent to this 'Vertex'
--      2. now it's not ajacent to /anything/
--
--   it will still be in the Topology.
--
--   This allows for the following identity to hold true for any given starting state @t@:
--
--   >>> execState (addFreeVertex >>= removeVertex) t == t
--   True
removeVertex :: Vertex -> TopoState ()
removeVertex (Vertex n) = do
    t <- gets unTopology
    put $ Topology $ Graph.delNode n t
    pure ()

addFreeEdge :: TopoState Edge
addFreeEdge = do
    e <- addNode EEdge
    pure $ Edge e

-- | Takes a "free" 'Edge' - that is, one with zero adjacencies - and creates a new
--   'Vertex' that it is adajacent to
--
--   We call it a "Ray Edge" because we can conceptually visualize the relationship of an
--   'Edge' with a single 'Vertex' as a "ray" that has a starting point but goes on
--   forever.
makeRayEdge :: Edge -> TopoState (Maybe Vertex)
makeRayEdge (Edge e)= do
    t <- get
    if isValidNode t e && not (hasNeighbors t e)
        then do
            v <- addNode EVertex
            t <- get
            let m = connectNodes (v, e) t
            case m of
                Just t' -> do put t'
                              pure $ Just (Vertex v)
                Nothing -> pure Nothing
        else
            pure Nothing

closeRayEdge :: Edge -> TopoState (Maybe Vertex)
closeRayEdge (Edge e) = do
    t <- get
    if isValidNode t e && ((length (Graph.neighbors (unTopology t) e)) == 1)
        then do
            v <- addNode EVertex
            t <- get
            let m = connectNodes (e, v) t
            case m of
                Just t' -> do put t'
                              pure $ Just (Vertex v)
                Nothing -> pure Nothing
        else
            pure Nothing

-- | If the Edge does not exist, does nothing.
--
--   __WARNING__
--   This __will__ delete any "hanging" adjacencies. In other words, if there was
--   something that:
--
--      1. /was/ adjacent to this Edge
--      2. now is not adjacent to /anything/
--
--   then it will also be deleted.
--
--   This allows for the following identity to hold true for any given starting state @t@:
--
--   >>> execState (addFreeEdge >> removeEdge) t == t
--   True
removeEdge :: Edge -> TopoState ()
removeEdge e = do
    let n = getEdgeID e
    t <- gets unTopology
    let vs = Graph.neighbors t n
        t' = Graph.delNode n t
        ns = filter ((== 0) . length . (Graph.neighbors t')) vs
    put $ Topology $ foldr Graph.delNode t' ns
    pure ()

vertexAdjacentEdges :: Topology -> Vertex -> [Edge]
vertexAdjacentEdges (Topology g) (Vertex n) = map Edge es
    where es = Graph.neighbors g n

edgeAdjacentVertices :: Topology -> Edge -> [Vertex]
edgeAdjacentVertices (Topology g) (Edge n) = map Vertex vs
    where vs = Graph.neighbors g n

-- | Which 'Vertex' are adjacent to this 'Edge'?
--   Results in an error if the Edge does not exist in the Topology
adjVertToEdge :: Topology -> Edge -> [Vertex]
adjVertToEdge t e = map Vertex ns
    where n  = getEdgeID e
          t' = unTopology $ getSubGraph (not . isFace) t
          ns = Graph.neighbors t' n

-- | Which 'Edge are adjacent to this 'Vertex'?
--   Results in an error if the Vertex does not exist in the Topology
adjEdgeToVert :: Topology -> Vertex -> [Edge]
adjEdgeToVert t (Vertex n) = map Edge ns
    where t' = unTopology $ getSubGraph (not . isFace) t
          ns = Graph.neighbors t' n

-- | Returns all the 'Vertex' in the 'Topology'
getVertices :: Topology -> [Vertex]
getVertices t = map Vertex $ getNodes isVertex t

-- | Returns all the 'Edge' in the 'Topology'
getEdges :: Topology -> [Edge]
getEdges t = map Edge $ getNodes isEdge t

-- | Returns all the 'Face' in the 'Topology'
getFaces :: Topology -> [Face]
getFaces t = map Face $ getNodes isFace t

prettyPrintVertex :: Topology -> Vertex -> Doc ann
prettyPrintVertex t (Vertex i) = prettyPrintNodeWithNeighbors t i

prettyPrintVertices :: Topology -> Doc ann
prettyPrintVertices t = prettyPrintMany t prettyPrintVertex getVertices

prettyPrintEdge :: Topology -> Edge -> Doc ann
prettyPrintEdge t (Edge i) = prettyPrintNodeWithNeighbors t i

prettyPrintEdges :: Topology -> Doc ann
prettyPrintEdges t = prettyPrintMany t prettyPrintEdge getEdges

prettyPrintFace :: Topology -> Face -> Doc ann
prettyPrintFace t (Face i) = prettyPrintNodeWithNeighbors t i

prettyPrintFaces :: Topology -> Doc ann
prettyPrintFaces t = prettyPrintMany t prettyPrintFace getFaces

prettyPrintTopology :: Topology -> Doc ann
prettyPrintTopology t = vs <> line <> es <> line <> fs
    where vs = prettyPrintVertices t
          es = prettyPrintEdges t
          fs = prettyPrintFaces t
-- ===========================================================================
--                        Private Free Functions
-- ===========================================================================
-- | The 'Node' is the basic building blocks of a 'Graph'. Any given Node can
--   have zero or more adjacencies.
--
--   While it may be tempting to call these nodes our topological \"Vertex", and
--   the adjacencies between them our topological \"Edge", this ends up muddying
--   things up. That's because, the concept of \"Vertex" and \"Edge" as it
--   pertains to graph theory are very distinct from those related to geometry
--   and CAD.
--
--   Therefore, to keep things straight, we're calling them Nodes and
--   \"Connections".
--
--   Any given Node can reperesent any of our Topological entities - namely, a
--   'Vertex', 'Edge' or 'Face'. The Connections between these will form the
--   necessary adjacency relationships to establish the information needed for
--   doing CAD stuff.
--
--   Please note that it is very important to control the exported API here -
--   using this system, it is possible to do anything. A 'Vertex' can be
--   connectiod to another 'Vertex' and a 'Face', or maybe two 'Face' and all
--   kinds of things that don't really make sense (or maybe they do make sense
--   but they're a real pain to deal with).
addNode :: Element -> TopoState Int
addNode e = do
    t <- get
    let t' = unTopology t
        n  = length $ Graph.nodes t'
        e' = (e, countNode f t)
        f | e == EVertex = isVertex
          | e == EEdge   = isEdge
          | e == EFace   = isFace
    put $ Topology $ Graph.insNode (n, e') t'
    pure n

-- | This relationship is directional - i.e. this will establish a relationship
-- __from__ @a@ __to__ @b@
connectNodes :: (Int, Int) -> Topology -> Maybe Topology
connectNodes (a, b) t = do
    let t' = unTopology t
    Just $ Topology $ Graph.insEdge (a, b, BridgeLabel ()) t'

isValidNode :: Topology -> Int -> Bool
isValidNode (Topology g) n = Graph.gelem n g

hasNeighbors :: Topology -> Int -> Bool
hasNeighbors (Topology g) n = (not . null) $ Graph.neighbors g n

-- | How many nodes are in the Topology matching this predicate?
countNode :: (NodeLabel -> Bool) -> Topology -> Int
countNode p = length . Graph.nodes . Graph.labfilter p . unTopology

-- | Predicate, useful for countNode, and probably others
isVertex :: NodeLabel -> Bool
isVertex (EVertex, _) = True
isVertex _ = False

-- | Predicate, useful for countNode, and probably others
isEdge :: NodeLabel -> Bool
isEdge (EEdge, _) = True
isEdge _ = False

-- | Predicate, useful for countNode, and probably others
isFace :: NodeLabel -> Bool
isFace (EFace, _) = True
isFace _ = False

getSubGraph :: (NodeLabel -> Bool) -> Topology -> Topology
getSubGraph p t = Topology $ Graph.labfilter p $ unTopology t

getNodes :: (NodeLabel -> Bool) -> Topology -> [Int]
getNodes p t = Graph.nodes $ unTopology $ getSubGraph p t

prettyPrintNodeWithNeighbors :: Topology -> Int -> Doc ann
prettyPrintNodeWithNeighbors t i = h <> pretty ":" <+> ns
    where h   = prettyPrintNodeLabel lab
          lab = fromJust $ Graph.lab t' i
          ns  = prettyPrintNeighbors i t
          t'  = unTopology t

prettyPrintMany ::
    Topology -> (Topology -> a -> Doc ann) -> (Topology -> [a]) -> Doc ann
prettyPrintMany t f g = vsep $ map (f t) $ g t

prettyPrintNeighbors :: Int -> Topology -> Doc ann
prettyPrintNeighbors i t = align doc
    where pns = Graph.pre t' i
          sns = Graph.suc t' i
          lns = [length pns, length sns]
          t'  = unTopology t
          doc | all (== 0) lns = pretty "Free"
              | otherwise = pre <> line <> suc
              where pre = prettyPrintNodes "<-" pns t
                    suc = prettyPrintNodes "->" sns t

prettyPrintNodes :: String -> [Int] -> Topology -> Doc ann
prettyPrintNodes h ns t = pretty h <+> (align . vsep) ns'
    where ns' | length ns == 0 = [pretty "None"]
              | otherwise = map prettyPrintNodeLabel $ mapMaybe (Graph.lab t') ns
          t' = unTopology t

prettyPrintNodeLabel :: NodeLabel -> Doc ann
prettyPrintNodeLabel (e, n) = prettyPrintElement e <> pretty n

prettyPrintElement :: Element -> Doc ann
prettyPrintElement EVertex = pretty "V"
prettyPrintElement EEdge   = pretty "E"
prettyPrintElement EFace   = pretty "F"

-- ===========================================================================
--                            Instances
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = elements [t0, t1, t2, t3, t4, t5, t6]
        where t0 = emptyTopology
              t1 = execState addFreeVertex emptyTopology -- Single free vertex
              t2 = execState addFreeVertex t1            -- Two free vertex
              t3 = execState addEdge t0                  -- Single HalfEdge
              t4 = execState addFreeVertex t2            -- HalfEdge plus two free Vertex
              t5 = execState addEdge t3                  -- Two independent HalfEdge
              t6 = execState addEdge t4                  -- two independent HalfEdge, two free Vertex
              addEdge = addFreeEdge >>= makeRayEdge' >>= closeRayEdge
              makeRayEdge' e = do
                    makeRayEdge e
                    pure e
