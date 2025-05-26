import System.Environment (getArgs)
import Route (Stop(..), readStops, readLines)
import Route (LineTable(..), LineStop(..))
import qualified Route as R
import RouteGUI
import Graph 
import qualified Graph as G
import SkewHeap
import qualified Data.Map as M

 -- Create a module and use a sensible graph representation

shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = 
  let visited = []  -- List of visited nodes
      pq = pQ [(from, 0)]  -- Priority queue initialized with the source node and distance 0
      distMap = M.singleton from 0  -- Map to keep track of distances from the source
      pathMap = M.singleton from []  -- Map to keep track of paths
  in dijkstra g visited pq distMap pathMap to
    -- If the destination is reachable, return the path and distance
  where
    dijkstra :: Ord a => Graph a b -> [a] -> SkewHeap (a, b) -> M.Map a b -> M.Map a [a] -> a -> Maybe ([a], b)
    dijkstra g visited pq distMap pathMap to
      | isEmptySkewHeap pq = Nothing  -- If the priority queue is empty, no path found
      | otherwise = case minNodeDist (toList pq) of
          Nothing -> Nothing  -- No nodes left to process
          Just (v, l) ->
            if v == to then Just (visNodes g visited ++ [to], l)  -- If we reached the destination, return the path and distance
            else if v `elem` visited then dijkstra g visited (deleteMinSkewHeap pq) distMap pathMap to  -- Skip already visited nodes
            else let newVisited = v : visited
                     neighbors = G.adj g v  -- Get adjacent nodes
                     newPQ = foldr (\(Edge _ w l1) acc ->
                                     if M.findWithDefault (2^31 - 1) w distMap > l + l'
                                      then insertSkewHeap (w, l + l1) acc
                                      else acc) (deleteMinSkewHeap pq) neighbors
                     newDistMap = M.insert v l distMap  -- Update distance map
                     newPathMap = M.insert v (visNodes g newVisited ++ [v]) pathMap  -- Update path map
                 in dijkstra g newVisited newPQ newDistMap newPathMap to  -- Continue with the next node
-- | Find the node with the minimum distance in a list of (node, distance) pairs.

minNodeDist :: Ord b => [(a, b)] -> Maybe (a, b)
minNodeDist [] = Nothing
minNodeDist xs = Just $ minimum xs  -- Find the node with the minimum distance

visNodes :: Ord a => Graph a b -> [a] -> [a]
visNodes g visited = filter (`elem` visited) (G.vertices g)

pQ :: Ord a => [(a, b)] -> SkewHeap (a, b)
pQ [] = emptySkewHeap
pQ xs = foldr insertSkewHeap emptySkewHeap xs


main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs

  -- Check if two arguments are provided
  if length args /= 4
    then putStrLn "Please provide four arguments."
  else do
    Right stops <- readStops $ head args  -- First text file argument
    Right lines <- readLines $ args !! 1  -- Second text file argument
    let src = args !! 2  -- Source stop
        dst = args !! 3  -- Destination stop

        -- Generate path from data
        graph = graphBuilder stops lines
        path  = shortestPath graph src dst
    case path of 
      Nothing -> print $ 2^31 -1
      Just (list,time) -> do
        print time
        putStr $ unlines list  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-air.txt"
  Right lines <- readLines "lines-air.txt"
  let graph = graphBuilder stops lines
  runGUI stops lines graph shortestPath

graphBuilder :: [Stop] -> [LineTable] -> Graph String Integer
graphBuilder stops lines = foldr addLineEdges (foldr G.addVertex G.empty stops) lines
  where
    addLineEdges :: LineTable -> Graph String Integer -> Graph String Integer
    addLineEdges (LineTable line noStops stops) g =
      let stopPairs = zip stops (tail stops)  -- Create pairs of consecutive stops
          edges = map (\(s1, s2) -> G.addBiEdge s1 s2 line g) stopPairs  -- Add edges for each pair
      in foldr id g edges  -- Fold over the edges to add them to the graph
-- | Add edges for each line in the graph, connecting consecutive stops with the line number as the label.