import Control.Exception
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import System.Environment
import System.Exit
import Text.Read

type Meters = Float
type Minutes = Float

-- 15km\h = 250 m/min
speed :: Float
speed = 250

-- 8h time limit in seconds
timeLimit :: Minutes
timeLimit = 8.0 * 60

data Node = Node
  { xCoord :: Meters
  , yCoord :: Meters
  , nodeReward :: Int
  }
  deriving (Show, Eq, Ord)

data Trip = Trip
  { start :: Node
  , end :: Node
  , tripReward :: Int
  , travelTime :: Minutes
  , relativeReward :: Float
  }
  deriving (Show, Eq)

mkNode :: String -> String -> String -> Node
mkNode x y r = Node (1000 * (read x :: Meters)) (1000 * (read y :: Meters)) (read r :: Int)

nodeFromList :: [String] -> Maybe Node
nodeFromList [a, b, c] = Just (mkNode a b c)
nodeFromList _ = Nothing

calcDistance :: Node -> Node -> Meters
calcDistance a b = sqrt ((yCoord b - yCoord a) ** 2 + (xCoord b - xCoord a) ** 2)

calcTravelTime :: Float -> Node -> Node -> Minutes
calcTravelTime mySpeed a b = calcDistance a b / mySpeed

magTravelTime :: Node -> Node -> Minutes
magTravelTime = calcTravelTime speed

mkTrip :: Node -> Node -> Trip
mkTrip nodeA nodeB =
  let travelTime = magTravelTime nodeA nodeB
   in Trip nodeA nodeB (nodeReward nodeB) travelTime (fromIntegral (nodeReward nodeB) / travelTime)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x

parseInput :: String -> (Maybe Int, Maybe [Node])
parseInput input =
  let inputLines = lines input
      numOfPatients = head' inputLines >>= readMaybe
      nodes = sequenceA $ map (nodeFromList . words) (tail inputLines)
   in (numOfPatients, nodes)

pairOfDifferentNodes :: Eq a => [a] -> [(a, a)]
pairOfDifferentNodes l = [(x, y) | x <- l, y <- l, x /= y]

addTripToMap :: Map.Map Node [Trip] -> Trip -> Map.Map Node [Trip]
addTripToMap nodeMap trip = Map.insertWith (++) (start trip) [trip] nodeMap

findNextMostProfitableNode :: [Trip] -> Node -> (Map.Map Node [Trip]) -> Maybe Trip
findNextMostProfitableNode history currentNode nodeMap =
  case Map.lookup currentNode nodeMap of
    Nothing -> Nothing
    Just trips -> head' $ filter (\x -> (end x) `notElem` (map end history)) $ sortOn (Data.Ord.Down . relativeReward) trips

travel :: Map.Map Node [Trip] -> [Trip] -> Node -> [Trip]
travel nodeMap history currentNode =
  case findNextMostProfitableNode history currentNode nodeMap of
    Nothing -> history
    Just nextTrip -> travel nodeMap (nextTrip : history) (end nextTrip)

calculateTrip :: Minutes -> [Trip] -> (Minutes, Int)
calculateTrip limit trips = case head' trips of
  Nothing -> (0, 0)
  Just trip ->
    let fullTour = reverse $ mkTrip (end trip) (Node 0 0 0) : trips
        fullTourTime = foldl' (+) 0 $ map travelTime fullTour
        fullTourReward = foldl' (+) 0 $ map tripReward fullTour
        result =
          if fullTourTime > limit
            then calculateTrip limit (tail trips)
            else (fullTourTime, fullTourReward)
     in result

main :: IO ()
main = do
  args <- getArgs
  inputOrExc <- try (readFile $ head args) :: IO (Either SomeException String)
  case inputOrExc of
    Left e -> do
      print e
      print "Exiting"
      exitWith (ExitFailure 1)
    Right input -> do
      let (maybeNumOfPatients, maybeNodes) = parseInput input
      if (isNothing maybeNodes || isNothing maybeNumOfPatients)
        then do
          print "Parsing Failed"
          exitWith (ExitFailure 1)
        else do
          let nodes = Node 0 0 0 : fromJust maybeNodes
          let numOfPatients = fromJust maybeNumOfPatients
          -- print $ take 4 nodes
          let trips = fmap (uncurry mkTrip) $ pairOfDifferentNodes nodes
          let nodeMap = Map.fromList $ map (\x -> (x, [])) nodes :: Map.Map Node [Trip]
          let filledMap = foldl' addTripToMap nodeMap trips
          -- let bestNextTripMap = Map.map (head) $ filledMap
          let trip = travel filledMap [] (Node 0 0 0)
          let result = calculateTrip timeLimit trip
          print $ snd result
          exitWith (ExitSuccess)
