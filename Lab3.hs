import Route
import RouteGUI
import Graph
import SkewHeap
 -- Create a module and use a sensible graph representation

shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = undefined
  -- TODO: implement Dijkstra's algorithm

-- minDist
-- pQ
-- visNodes


main :: IO ()
main = undefined  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-air.txt"
  Right lines <- readLines "lines-air.txt"
  let graph = empty -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath