-- ###################
-- Advent Of Code 2016
-- ###################
-- Challenge 1
-- ###################

-- Disclaimer: This code will be terrible, as this is my first foray into Haskell
-- Published under WTFPL, original author Spoffy.

import qualified Data.Text as T

data Direction = North | East | South | West deriving (Show)
data Turn = TLeft | TRight deriving (Show)
data MoveOrder = MoveOrder { turnDirection :: Turn
                           , distance :: Int
                           } deriving (Show)
                           
toTurn 'L' = TLeft
toTurn 'R' = TRight
toTurn _ = TLeft -- Really, this should have a Nil value, or be wrapped in a Maybe?

--Cleaning up and preprocessing input to
delimiter = T.singleton ','

removeSpaces :: T.Text -> T.Text
removeSpaces input = T.filter ((/=) ' ') input

tokenise :: T.Text -> [T.Text]
tokenise input = T.splitOn delimiter $ removeSpaces input

-- !!!!!!!!!!!!!!!!!!!!!!!!TODO
tokenToMoveOrder :: String -> MoveOrder
tokenToMoveOrder token = MoveOrder {turnDirection=(toTurn $ head token), distance=(read (tail token))}

preprocess = (map tokenToMoveOrder) . (map T.unpack) . tokenise . T.pack

--Handling movement
--ChangeDirection could probably be done using Enums and succ pred
changeDirection :: Direction -> Turn -> Direction
changeDirection North TLeft = West
changeDirection West TLeft = South
changeDirection South TLeft = East
changeDirection East TLeft = North
changeDirection North TRight = East
changeDirection East TRight = South
changeDirection South TRight = West
changeDirection West TRight = North

walkDistance (currentDirection, x, y) distance = case currentDirection of North -> (currentDirection, x, y + distance)
                                                                          East -> (currentDirection, x + distance, y)
                                                                          South -> (currentDirection, x, y - distance)
                                                                          West -> (currentDirection, x - distance, y)

applyMoveOrder (currentDirection, x, y) order = let newDirection = changeDirection currentDirection (turnDirection order)
                                                    newPosition = (newDirection, x, y)
                                                in
                                                walkDistance newPosition (distance order)

applyMoveOrders :: [MoveOrder] -> (Direction, Int, Int)
applyMoveOrders orders = foldl applyMoveOrder startingPosition orders
  where startingPosition = (North, 0, 0)
  
calculateDistanceMoved input = let (currentDirection, x, y) = applyMoveOrders $ preprocess input
                               in x + y
  
main = do   
    line <- getLine  
    print $ calculateDistanceMoved line
   
  
