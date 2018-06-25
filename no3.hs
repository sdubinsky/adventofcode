-- The largest number(m) in a square is in the bottom right corner.  It's a perfect square of an odd number.  x is in that number's square if it's smaller than that number but larger than the next smallest perfect square.
-- x is on the bottom row if x > (m-(sqrt(m)-1))
-- x is on the left edge if x is not on the bottom row and x > (m-2(sqrt(m)-1))
-- and etc for the top and right edges.
-- y = 25, corners are: 25 - 5 - 1 = 21, 25 - (10 - 2) = 17, 25 - (15 - 3) = 13.
-- if x == y, subtract one from x and try again.
-- if x is on a corner, add one and try again.
-- Given x is on edge e, find the distance d between x and the next lower corner number.
-- Find the equivalent corner c in the square below.
-- Set x equal to c + d - 1

type Corner = Int -- a corner
type Square = Int -- The number that completes a square
type Location = Int -- The current location
type LocationValue = Int -- the value of the memory location for part 2
data CornerName = UpperLeft | UpperRight | LowerLeft | LowerRight deriving (Eq, Show)

findCorner :: Int -> Square -> Corner
findRelevantCorner :: CornerName -> (Square -> Corner)
getCorner :: Square -> Location -> Corner
findSquare :: Location -> Square
findNextSquare :: Location -> Square
findNextLocation :: Location -> Location
findMemoryValue :: Location -> LocationValue
isACorner :: Location -> Bool

isACorner loc = 
  let sq = findSquare loc in
    loc == findRelevantCorner LowerLeft sq ||
    loc == findRelevantCorner UpperLeft sq ||
    loc == findRelevantCorner UpperRight sq ||
    loc == sq
    
findCorner x sq = sq - x * ((floor $ sqrt $ fromIntegral sq) - 1)

findRelevantCorner name
  | name == LowerLeft = findCorner 1
  | name == UpperLeft = findCorner 2
  | name == UpperRight = findCorner 3
  | name == LowerRight = findCorner 4


getCornerName sq loc
  | findRelevantCorner LowerLeft sq <= loc = LowerLeft 
  | findRelevantCorner UpperLeft sq <= loc = UpperLeft 
  | findRelevantCorner UpperRight sq <= loc = UpperRight 
  | otherwise = LowerRight

getCorner sq loc
  | sq == loc = sq
  | findRelevantCorner LowerLeft sq <= loc = findRelevantCorner LowerLeft sq
  | findRelevantCorner UpperLeft sq <= loc = findRelevantCorner UpperLeft sq
  | findRelevantCorner UpperRight sq <= loc = findRelevantCorner UpperRight sq
  | otherwise = findNextSquare loc

findNextSquare loc =
  let root = floor $ sqrt(fromIntegral loc) in
    (root - ((root - 1) `mod` 2)) ^ 2

findSquare loc =
  let root = ceiling $ sqrt(fromIntegral loc) in
    (root + ((root + 1) `mod` 2)) ^ 2

findNextLocation loc
  | loc == 1 = 0
  | loc == 2 = 1
  | loc == 4 = 1
  | loc == 6 = 1
  | loc == 8 = 1
  
findNextLocation loc = 
  let
    sq = findSquare loc;
    newSq = findNextSquare loc;
    corner = getCorner sq loc;
    dist = loc - corner - 1;
    cornerName = getCornerName sq loc
    newCorner = findRelevantCorner cornerName newSq
  in
    if corner == loc
    then loc - 1
    else newCorner + dist

manhattanDistance :: Location -> Int -> Int

manhattanDistance loc count
  | loc == 0 = 0
  | loc == 1 = count
  | otherwise = manhattanDistance (findNextLocation loc) count + 1

findMemoryValue loc 
  | loc <= 0 = 0
  | loc == 1 = 1
  | loc == 2 = 1
  | loc == 3 = 2
  | loc == 4 = 4
  | loc == 5 = 5
  | loc == 6 = 10

--Special cases:
--4 corners
--next to a corner
findMemoryValue loc =
  let sq = findSquare loc in
    --lower right corner
    if loc == sq
    then findMemoryValue(loc - 1) + findMemoryValue(findNextLocation(loc - 1)) + findMemoryValue(findNextLocation(loc - 1) + 1)
    else if loc == sq - 1
    then findMemoryValue(loc - 1) + findMemoryValue(findNextLocation(loc - 1)) + findMemoryValue(findNextLocation(loc)) + findMemoryValue(findNextLocation(loc) + 1)
    else if loc == findNextSquare(loc) + 1
    then findMemoryValue(loc - 1) + findMemoryValue(findNextLocation(loc + 1))
    else if loc == findNextSquare(loc) + 2
    then findMemoryValue(loc - 1) + findMemoryValue(loc - 2) + findMemoryValue(findNextLocation(loc)) + findMemoryValue(findNextLocation(loc + 1))
    --other corners
    else if isACorner loc
    then findMemoryValue(loc - 1) + findMemoryValue(findNextLocation(loc - 1))
    else if isACorner(loc - 1)
    then findMemoryValue(loc - 1) + findMemoryValue(loc - 2) + findMemoryValue(findNextLocation(loc)) + findMemoryValue(findNextLocation(loc) + 1)
    else if isACorner(loc + 1)
    then findMemoryValue(loc - 1) + findMemoryValue(findNextLocation(loc - 1)) + findMemoryValue(findNextLocation(loc))
    else findMemoryValue(loc - 1) + findMemoryValue(findNextLocation(loc)) + findMemoryValue(findNextLocation(loc) + 1) + findMemoryValue(findNextLocation(loc) - 1)

findAnswer x
  | findMemoryValue(x) > 312051 = x
  | otherwise = findAnswer(x + 1)
main :: IO()
main = print $ show $ findNextLocation 8

