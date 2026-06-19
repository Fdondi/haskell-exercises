module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot{ bearing:: Bearing, coordinates:: (Integer, Integer)} deriving (Eq, Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction coordinates

advanceBearing North = East
advanceBearing East = South
advanceBearing South = West
advanceBearing West = North

turnRight robot@Robot{bearing=bearing} = robot{bearing = advanceBearing bearing} 

regressBearing East = North
regressBearing South = East
regressBearing West = South
regressBearing North = West

turnLeft robot@Robot{bearing=bearing} = robot{bearing = regressBearing bearing}

updateCoordinates North (x, y) = (x, y + 1)
updateCoordinates South (x, y) = (x, y - 1)
updateCoordinates East  (x, y) = (x + 1, y)
updateCoordinates West  (x, y) = (x - 1, y)

advance robot@Robot{bearing = dir} = robot { coordinates = updateCoordinates dir (coordinates robot) }

moveOne :: Char -> Robot -> Robot
moveOne 'R' robot = turnRight robot
moveOne 'L' robot = turnLeft robot
moveOne 'A' robot = advance robot

move :: Robot -> String -> Robot
move robot []     = robot
move robot (i:is) = move (moveOne i robot) is
