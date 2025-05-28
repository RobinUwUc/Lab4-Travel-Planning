module Graph 
  ( -- * Edge
    Edge(..)                    -- type
  , src, dst, label         -- querying an Edge

    -- * Graph
  , Graph                   -- type
  , empty                   -- create an empty map
  , addVertex, addVertices  -- adding vertices (nodes)
  , addEdge, addBiEdge      -- adding edges (one way or both ways)
  , adj                     -- get adjacent nodes for a given node
  , vertices, edges         -- querying a Graph
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- An edge with a source and destination node (of type a), 
-- together with a label of type b
data Edge a b = Edge
  { src   :: a  -- ^ Source vertex
  , dst   :: a  -- ^ Destination vertex
  , label :: b  -- ^ The label
  } deriving (Show, Eq, Ord)

-- A graph with nodes of type a and labels of type b.
data Graph a b = Graph 
  { adjmap       :: M.Map a [Edge a b]
  } deriving Show


graph1 = M.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]

-- | Create an empty graph
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v g
  | M.member v (adjmap g) = g  -- Vertex already exists, do nothing
  | otherwise = g { adjmap = M.insert v [] (adjmap g)}  -- Add vertex with empty adjacency list

-- | Add a list of vertices to a graph
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices [] g = g
addVertices vs g = foldr addVertex g vs

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l g
  | M.member v (adjmap g) && M.member w (adjmap g) = 
      g { adjmap = M.adjust ((Edge v w l):) v (adjmap g) }
  | otherwise = g  -- One of the vertices does not exist, do nothing

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l = addEdge v w l . addEdge w v l

-- | Get all adjacent vertices (nodes) for a given node
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v g = M.findWithDefault [] v (adjmap g)

-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices g = M.keys (adjmap g)

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges g = concat (M.elems (adjmap g))
