{-# LANGUAGE ViewPatterns        #-}
module Main where

main :: IO ()
main = day12

day12 :: IO ()
day12 = do
  putStrLn "day 12"
  actions <- fmap parseAction . lines <$> readFile "day12.txt"
  let test = fmap parseAction (lines "F10\nN3\nF7\nR90\nF11")
  print $ manhattan (navigateShip actions)
  print $ manhattan (navigateShipWaypoint actions)

navigateShip :: [Action] -> Ship
navigateShip = foldl guideShip startShip

navigateShipWaypoint :: [Action] -> Ship
navigateShipWaypoint actions = fst (go actions)
  where
    go = foldl guideShipWayPoint (startShip, startWaypoint)

data Dir = N | E | W | S
  deriving (Show, Eq)

type Action = (Char, Int)

parseAction :: String -> (Char,Int)
parseAction (action:amount) = (action, read amount)

guideShip :: Ship -> Action -> Ship
guideShip ship (action, amount) = do
  let (x,y) = coords ship
  ship {
    coords =
       case action of
         'F' ->
            case facing ship of
              N -> (x, y + amount)
              E -> (x + amount, y)
              W -> (x - amount, y)
              S -> (x, y - amount)
         'N' -> (x, y + amount)
         'E' -> (x + amount, y)
         'W' -> (x - amount, y)
         'S' -> (x, y - amount)
         _   -> (x, y)
   , facing =
      case amount of
        90 ->
          case action of
           'R' ->
             case facing ship of
               N -> E
               E -> S
               S -> W
               W -> N
           'L' ->
             case facing ship of
               N -> W
               E -> N
               S -> E
               W -> S
           _ -> facing ship
        180 ->
          facing
            (iterate
               (flip guideShip (action,90)) ship !! 2)
        270 ->
          facing
            (iterate
               (flip guideShip (action,90)) ship !! 3)
        _ ->
          facing ship
   }

data Ship
  = Ship
  { facing :: Dir
  , coords :: (Int,Int)
  } deriving (Show, Eq)

startShip :: Ship
startShip = Ship E (0,0)

manhattan :: Ship -> Int
manhattan (coords -> (x,y)) = abs x + abs y

type Waypoint = (Int,Int)

startWaypoint :: Waypoint
startWaypoint = (10,1)

guideShipWayPoint
  :: (Ship, Waypoint)
  -> Action
  -> (Ship, Waypoint)
guideShipWayPoint (ship, (x,y)) (action, amount) =
  case action of
    'F' -> (movedShip, (x,y))
    'N' -> (ship, (x,y+amount))
    'E' -> (ship, (x+amount,y))
    'W' -> (ship, (x-amount,y))
    'S' -> (ship, (x,y-amount))
    _   ->
     case amount of
       90 ->
         case action of
           'R' -> (ship, (y, negate x))
           'L' -> (ship, (negate y, x))
       180 ->
         iterate
           (flip guideShipWayPoint
              (action, 90)) (ship, (x,y)) !! 2
       270 ->
         iterate
           (flip guideShipWayPoint
              (action, 90)) (ship, (x,y)) !! 3
    where
      (shipX, shipY) = coords ship
      movedShip =
        ship { coords =
                 ( shipX + (x * amount)
                 , shipY + (y * amount)
                 )
             }
