import System.Environment (getArgs)
import Route (Stop(..), readStops, readLines)
import Route (LineTable(..), LineStop(..))
import qualified Route as R
import RouteGUI
import Graph 
import qualified Graph as G
import SkewHeap
import qualified SkewHeap as SH
import qualified Data.Map as M
import Data.List (minimumBy)

 -- Create a module and use a sensible graph representation

shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to
  | from == to = Just ([from], 0)
  | otherwise = dijkstra initialPQ M.empty (M.singleton from 0)
  where
    initialPQ = SH.insert (from, 0) SH.empty'
    dijkstra pq preds dists =
      case SH.findMin pq of
        Nothing -> Nothing
        Just (current, currentDist)
          | currentDist > M.findWithDefault (2^31 -1) current dists -> 
              dijkstra (SH.delete (current, currentDist) pq) preds dists
          | current == to -> Just (reconstructPath current preds, currentDist)
          | otherwise ->
              let
                neighbors = adj current g

                update (pq', dists', preds') (Edge _ neighbor cost) =
                  let newDist = currentDist + cost
                      currentBest = M.findWithDefault (2^31 -1) neighbor dists'
                  in
                    if newDist < currentBest
                      then (SH.insert (neighbor, newDist) pq',
                            M.insert neighbor newDist dists',
                            M.insert neighbor current preds')
                      else (pq', dists', preds')

                (newPQ, newDists, newPreds) = 
                  foldl update (SH.delete (current, currentDist) pq, dists, preds) neighbors
              in dijkstra newPQ newPreds newDists

    reconstructPath node preds = reverse $ go node []
      where
        go n acc = case M.lookup n preds of
          Nothing -> n : acc
          Just prev -> go prev (n : acc)
                  
minNodeDist :: Ord b => [(a, b)] -> Maybe (a, b)
minNodeDist [] = Nothing
minNodeDist xs = Just $ minimumBy (\(_, d1) (_, d2) -> compare d1 d2) xs -- Find the node with the minimum distance

visited :: Ord a => Graph a b -> [a] -> [a]
visited g visited = filter (`elem` visited) (G.vertices g)

pQ :: (Ord a, Ord b) => [(a, b)] -> SkewHeap (a, b)
pQ xs = case xs of
  [] -> SH.empty'
  _  -> foldr SH.insert SH.empty' xs

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
        putStr $ unlines (reverse list)

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-gbg.txt"
  Right lines <- readLines "lines-gbg.txt"
  let graph = graphBuilder stops lines
  runGUI stops lines graph shortestPath

graphBuilder :: [Stop] -> [LineTable] -> G.Graph String Integer
graphBuilder stops lines =
  foldr addLineEdges (foldr G.addVertex G.empty (map stopToName stops)) lines
  where
    stopToName (Stop name _) = name

    addLineEdges :: LineTable -> G.Graph String Integer -> G.Graph String Integer
    addLineEdges (LineTable _ lineStops) g =
      let
    -- Extract stop names and their cumulative times from the start of the line
        stopNames = map stopName lineStops
        cumulativeTimes = scanl1 (+) (map time lineStops)  -- e.g. [0,3,5,12]

    -- Create stop pairs along with the actual travel time between them
        stopPairsWithCost =
          zip3 stopNames (tail stopNames) (zipWith (-) (tail cumulativeTimes) cumulativeTimes)

    -- Add bi-directional edges with travel time as weight
        edges = map (\(s1, s2, cost) -> \g -> G.addBiEdge s1 s2 cost g) stopPairsWithCost
      in
        foldr ($) g edges