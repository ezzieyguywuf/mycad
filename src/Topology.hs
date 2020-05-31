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
, Vertex
, Face
, Edge
  -- * Mutating functions
, emptyTopology
, addVertex
, makeEdge
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

-- ===========================================================================
--                               Data Types
-- ===========================================================================
newtype Topology =
    Topology {unTopology :: Gr NodeLabel BridgeLabel}
        deriving (Show)

newtype Vertex = Vertex Int deriving (Show, Eq)
newtype Edge = Edge Int deriving (Show, Eq)
newtype Face = Face Int deriving (Show, Eq)

type NodeLabel = (Element, Int)
newtype BridgeLabel = BridgeLabel () deriving (Show)
data Element = EVertex | EEdge | EFace deriving (Show, Eq)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
-- | The EmptyTopology is the starting point for any 'Topology'. As no
--   constructors for 'Topology' are exported, this is the only way to create
--   one.
emptyTopology :: Topology
emptyTopology = Topology Graph.empty

-- | Adds a single "free" 'Vertex' to the 'Topology'. In this context, "free"
--   means that it is does not have any entities adjacent to it.
addVertex :: Topology -> Topology
addVertex = addNode EVertex

-- | An 'Edge' is always created between two 'Vertex'.
--   Any given 'Edge' is directional - therefore, an Edge can be created from v1
--   to v2 and from v2 to v1.
--
--   However, multiple Edges cannot be created in the same direction between two
--   Vertex - if this is attempted, an unmodified 'Topology' is returned.
--
--   Because of this, the maximum number of Edge between any two Vertex is 2.
makeEdge :: Vertex -> Vertex -> Topology -> Topology
makeEdge v1@(Vertex v1') v2@(Vertex v2') t
    | hasAny es1 es2 = t
    | otherwise = foldr connectNodes t' [(v1', e), (e, v2')]
    where t' = addNode EEdge t
          (Edge e)  = last $ getEdges t'
          es1 = adjEdgeToVert v1 t
          es2 = adjEdgeToVert v2 t

-- | Which 'Vertex' are adjacent to this 'Edge'?
--   Results in an error if the Edge does not exist in the Topology
adjVertToEdge :: Edge -> Topology -> [Vertex]
adjVertToEdge (Edge n) t = map Vertex ns
    where t' = unTopology $ getSubGraph (not . isFace) t
          ns = Graph.neighbors t' n

-- | Which 'Edge are adjacent to this 'Vertex'?
--   Results in an error if the Vertex does not exist in the Topology
adjEdgeToVert :: Vertex -> Topology -> [Edge]
adjEdgeToVert (Vertex n) t = map Edge ns
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
addNode :: Element -> Topology -> Topology
addNode e t = Topology $ Graph.insNode (n, e') t'
    where t' = unTopology t
          n  = length $ Graph.nodes t'
          e' = (e, countNode f t)
          f | e == EVertex = isVertex
            | e == EEdge   = isEdge
            | e == EFace   = isFace

connectNodes :: (Int, Int) -> Topology -> Topology
connectNodes (a, b) t =
    Topology $ Graph.insEdge (a, b, BridgeLabel ()) $ unTopology t

countNode :: (NodeLabel -> Bool) -> Topology -> Int
countNode p t = length . Graph.nodes $ Graph.labfilter p $ unTopology t

isVertex :: NodeLabel -> Bool
isVertex (EVertex, _) = True
isVertex _ = False

isEdge :: NodeLabel -> Bool
isEdge (EEdge, _) = True
isEdge _ = False

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

-- stolen from https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/Data-List-Utils.html#v:hasAny
{- | Returns true if the given list contains any of the elements in the search
list. -}
hasAny :: Eq a => [a]           -- ^ List of elements to look for
       -> [a]                   -- ^ List to search
       -> Bool                  -- ^ Result
hasAny [] _          = False             -- An empty search list: always false
hasAny _ []          = False             -- An empty list to scan: always false
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

-- ===========================================================================
--                            Instances
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = elements [t0, t1, t2, t3, t4]
        where t0 = emptyTopology
              t1 = addVertex emptyTopology
              t2 = addVertex t1
              t3 = makeEdge (Vertex 0) (Vertex 1) t2
              t4 = addVertex $ addVertex t3
