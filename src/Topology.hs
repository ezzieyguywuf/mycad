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
, addVertex
, addVertex'
, addEdge
  -- * Inspection functions
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
newtype Edge = Edge Int deriving (Show, Eq)
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
addVertex :: Topology -> Topology
addVertex = addNode EVertex

-- | Uses TopoState to add a Vertex and return the added Vertex
addVertex' :: TopoState Vertex
addVertex' = do
    n <- addNode' EVertex
    return $ Vertex n

-- | An 'Edge' is always created between two 'Vertex'.
--   Any given 'Edge' is directional - therefore, an Edge can be created from v1
--   to v2 and from v2 to v1.
--
--   However, multiple Edges cannot be created in the same direction between two
--   Vertex - if this is attempted, an unmodified 'Topology' is returned.
--
--   Because of this, the maximum number of Edge between any two Vertex is 2.

--  Note
--  ====
--  This is implementation stuff, that's why it's not in the exported
--  documentation. Feel free to skip this if you're just perusing.
--
--  Implementation stuff
--  ====================
--   Here's what it looks like after creating an Edge on an empty 'Topology'
--   using 'addEdge'
--
-- >>> let t = addEdge emptyTopology
-- >>> prettyPrintEdge t (getEdges t !! 0)
-- V0: <- None
--     -> E0
-- V1: <- E0
--     -> None
-- E0: <- V0
--     -> V1
--
-- In other words, the relationships go __from__ @V0@ __to__ @E0@, and finally
-- __from__ @E0@ __to__ @V1@.
--
-- Some might call this a \"Half-Edge" (others a \"Winged Edge""), as there is
-- still room to go back the other way from @V1@ to @V0@ by way of @E0@. Either
-- way, the Edge cannot really be considered "complete" until it has it's
-- sibling going the other way.
--
-- Why, then, not build it this way when 'makeEdge' is called?
--
-- Because (I think) doing it like this will allow for a user to create a
-- standalone 2-dimensional 'Face' surrounded by a loop of 'Edge', and then
-- later weave this 'Face' into a solib by attaching other 'Face' along the
-- half-edges.

-- | Adds a single "free" 'Edge' to the 'Topology'. In this context, "free" means that the
--   'Vertex' bounding the 'Edge' are only adjacent to the 'Edge' itself, and the 'Edge'
--   is not adjacent to anything but its two 'Vertex'
addEdge :: TopoState Edge
addEdge = do
    (Vertex v1) <- addVertex'
    (Vertex v2) <- addVertex'
    e  <- addNode' EEdge
    t  <- get
    let t' = foldr connectNodes t [(v1, e), (e, v2)]
    put t'
    return $ Edge e

-- | Which 'Vertex' are adjacent to this 'Edge'?
--   Results in an error if the Edge does not exist in the Topology
adjVertToEdge :: Topology -> Edge -> [Vertex]
adjVertToEdge t (Edge n) = map Vertex ns
    where t' = unTopology $ getSubGraph (not . isFace) t
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
addNode :: Element -> Topology -> Topology
addNode e t = Topology $ Graph.insNode (n, e') t'
    where t' = unTopology t
          n  = length $ Graph.nodes t'
          e' = (e, countNode f t)
          f | e == EVertex = isVertex
            | e == EEdge   = isEdge
            | e == EFace   = isFace

addNode' :: Element -> TopoState Int
addNode' e = do
    t <- get
    let t' = unTopology t
        n  = length $ Graph.nodes t'
        e' = (e, countNode f t)
        f | e == EVertex = isVertex
          | e == EEdge   = isEdge
          | e == EFace   = isFace
    put $ Topology $ Graph.insNode (n, e') t'
    return n

-- | This relationship is directional - i.e. this will establish a relationship
-- __from__ @a@ __to__ @b@
connectNodes :: (Int, Int) -> Topology -> Topology
connectNodes (a, b) t =
    Topology $ Graph.insEdge (a, b, BridgeLabel ()) $ unTopology t

-- | How many nodes are in the Topology matching this predicate?
countNode :: (NodeLabel -> Bool) -> Topology -> Int
countNode p t = length . Graph.nodes $ Graph.labfilter p $ unTopology t

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
    arbitrary = elements [t0, t1, t2, t4] --, t3, t4, t5, t6]
        where t0 = emptyTopology
              t1 = addVertex emptyTopology    -- Single free vertex
              t2 = addVertex t1               -- Two free vertex
              --t3 = addEdge t0                 -- Single 'free' Edge
              t4 = addVertex $ addVertex t2   -- free Edge plus two free Vertex
              --t5 = addEdge t2                 -- two free Edge
              --t6 = addEdge t4                 -- two free Edge, two free Vertex
