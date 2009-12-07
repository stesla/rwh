import Data.List

data Direction = LeftTurn
               | RightTurn
               | Straight
                 deriving Show

data Point = Point Double Double
             deriving (Eq, Show)

turn :: Point -> Point -> Point -> Direction
turn (Point ax ay) (Point bx by) (Point cx cy) = case compare 0 crossProduct of
  LT -> LeftTurn
  GT -> RightTurn
  EQ -> Straight
  where crossProduct = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

turns :: [Point] -> [Direction]
turns (a:b:c:xs) = (turn a b c):turns (b:c:xs)
turns _ = []

convexHull :: [Point] -> [Point]
convexHull points = findPath (p:others) []
  where p = start (head points) (tail points)
          where start point [] = point
                start (Point x y) (Point x' y':points) = start lower points
                  where lower = case compare y y' of
                          LT -> Point x y
                          GT -> Point x' y'
                          EQ -> case compare x x' of
                            GT -> Point x' y'
                            _ -> Point x y -- If they're exactly the same, who cares?
        others = sortBy angleFromXWithP (delete p points)
          where angleFromXWithP a b = compare (cotangent p a) (cotangent p b)
                  where cotangent (Point x y) (Point x' y') = (x'-x)/(y'-y)
        findPath (a:b:c:xs) path = case turn a b c of
          RightTurn -> findPath (a:c:xs) path
          _ -> findPath (b:c:xs) (a:path)
        findPath _ result = result
