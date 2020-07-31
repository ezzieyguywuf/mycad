-- * Pretty printing
-- | In the output, '->' means "out of" and '<-' means "in to". Therefore:
--
-- >>> prettyPrintVertex v t
-- V0: <- None
--     -> E0
--
-- Means that the 'Vertex' @V0@ has __no__ entities ('Edge' or otherwise)
-- pointing in to it, and a single 'Edge' named @E0@ pointing out of it.
module Topology.PrettyPrint
(
  prettyPrintVertex
, prettyPrintEdge
, prettyPrintTopology
)where
-- from: Tanisha
--beautifulPrintEdge.run

-- Base
import Data.Maybe (catMaybes)

-- Third-Party
import Data.Text.Prettyprint.Doc (Doc, pretty, vsep, align)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State(evalState, State)

-- Internal
import Topology ( Vertex, Edge, Adjacency(..), TopoState, Topology
                , vertexEdges, edgeVertices, vertexID, edgeID, unAdjacency
                , emptyTopology, getVertices, getEdges)

-- | The document type used in our pretty printer
type TopoDoc = Doc ()

prettyShowEdge :: Edge -> TopoState (Maybe TopoDoc)
prettyShowEdge edge = runMaybeT $ do
    gid <- MaybeT (edgeID edge)
    pure (pretty $ "Edge" <> show gid)

prettyShowVertex :: Vertex -> TopoState (Maybe TopoDoc)
prettyShowVertex vertex = runMaybeT $ do
    gid <- MaybeT (vertexID vertex)
    pure (pretty $ "Vertex" <> show gid)

prettyPrintAdjacency :: Adjacency a -> TopoDoc
prettyPrintAdjacency adjacency =
    case adjacency of
        In    _ -> pretty (" ← ")
        Out   _ -> pretty (" → ")
        InOut _ -> pretty (" ↔ ")

prettyPrintVertex :: Vertex -> TopoState (Maybe TopoDoc)
prettyPrintVertex vertex = runMaybeT $ do
    showV           <- MaybeT (prettyShowVertex vertex)
    edgeAdjacencies <- lift (vertexEdges vertex)
    let getMaybePPEdges = sequence . fmap (prettyShowEdge . unAdjacency)
    maybePPEdges    <- lift $ getMaybePPEdges edgeAdjacencies
    let ppAdjacencies = fmap prettyPrintAdjacency edgeAdjacencies
        ppEdges       = catMaybes maybePPEdges
    pure (showV <> (align . vsep) (zipWith (<>) ppAdjacencies ppEdges))

prettyPrintEdge :: Edge -> TopoState (Maybe TopoDoc)
prettyPrintEdge edge = runMaybeT $ do
    showE             <- MaybeT (prettyShowEdge edge)
    vertexAdjacencies <- lift (edgeVertices edge)
    let getMaybePPVertices = sequence . fmap (prettyShowVertex . unAdjacency)
    maybePPVertices   <- lift $ getMaybePPVertices vertexAdjacencies
    let ppAdjacencies = fmap prettyPrintAdjacency vertexAdjacencies
        ppVertices    = catMaybes maybePPVertices
    pure (showE <> (align . vsep) (zipWith (<>) ppAdjacencies ppVertices))

prettyPrintTopology :: Topology -> TopoDoc
prettyPrintTopology topology
    | topology == emptyTopology = pretty "empty topology"
    | otherwise =
        case evalState maybeDoc topology of
            Just doc -> doc
            Nothing  -> pretty "PrettyPrint error. TODO: Use Either"
        where maybeDoc = runMaybeT $ do
                  vs <- lift getVertices :: MaybeT (State Topology) [Vertex]
                  es <- lift getEdges
                  vDocs <- lift (sequence . fmap prettyPrintVertex $ vs)
                  eDocs <- lift (sequence . fmap prettyPrintEdge $ es)
                  let vDocs' = catMaybes vDocs
                      eDocs' = catMaybes eDocs
                  pure (vsep $ vDocs' <> eDocs')  :: MaybeT (State Topology) TopoDoc


--prettyPrintVertex :: Topology -> Vertex -> Doc ann
--prettyPrintVertex t (Vertex i) = prettyPrintNodeWithNeighbors t i

--prettyPrintEdge :: Topology -> Edge -> Doc ann
--prettyPrintEdge t (Edge i) = prettyPrintNodeWithNeighbors t i

--prettyPrintFace :: Topology -> Face -> Doc ann
--prettyPrintFace t (Face i) = prettyPrintNodeWithNeighbors t i

--prettyPrintTopology :: Topology -> Doc ann
--prettyPrintTopology t =
    --case show doc of
        --"" -> pretty "empty topology"
        --_  -> doc
    --where vs = prettyPrintMany t prettyPrintVertex getVertices
          --es = prettyPrintMany t prettyPrintEdge getEdges
          --fs = prettyPrintMany t prettyPrintFace getFaces
          --func d = if show d == "" then d else d <> line
          --doc = foldMap func [vs,es,fs]

-- ===========================================================================
--                        Private Free Functions
-- ===========================================================================
--prettyPrintNodeWithNeighbors :: Topology -> Int -> Doc ann
--prettyPrintNodeWithNeighbors t i = h <> pretty ":" <+> ns
    --where h   = prettyPrintNodeLabel lab
          --lab = fromJust $ Graph.lab t' i
          --ns  = prettyPrintNeighbors i t
          --t'  = unTopology t

--prettyPrintMany ::
    --Topology -> (Topology -> a -> Doc ann) -> (Topology -> [a]) -> Doc ann
--prettyPrintMany topo printer getter = vsep $ map (printer topo) $ getter topo

--prettyPrintNeighbors :: Int -> Topology -> Doc ann
--prettyPrintNeighbors i t = align doc
    --where pns = Graph.pre t' i
          --sns = Graph.suc t' i
          --lns = [length pns, length sns]
          --t'  = unTopology t
          --doc | all (== 0) lns = pretty "No neighbors"
              -- | otherwise = pre <> line <> suc
              --where pre = prettyPrintNodes "<-" pns t
                    --suc = prettyPrintNodes "->" sns t

--prettyPrintNodes :: String -> [Int] -> Topology -> Doc ann
--prettyPrintNodes h ns t = pretty h <+> (align . vsep) ns'
    --where ns' | length ns == 0 = [pretty "None"]
              -- | otherwise = map prettyPrintNodeLabel $ mapMaybe (Graph.lab t') ns
          --t' = unTopology t

--prettyPrintNodeLabel :: NodeLabel -> Doc ann
--prettyPrintNodeLabel (e, n) = prettyPrintElement e <> pretty n

--prettyPrintElement :: Element -> Doc ann
--prettyPrintElement EVertex = pretty "V"
--prettyPrintElement EEdge   = pretty "E"
--prettyPrintElement EFace   = pretty "F"

