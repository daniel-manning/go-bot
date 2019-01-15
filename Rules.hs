--import Data.Set
import Data.List

data Player = Black | White deriving (Show, Eq)

data Point = Point Int Int deriving (Show, Eq)

data Move = Play Point | Pass | Resign deriving Show

data Board = Board Int [ConnectedGroup]

neighbourhood (x, y) = [(x -1, y), (x, y + 1), (x, y - 1), (x + 1, y)]

data ConnectedGroup = Group Player [Point] [Point] deriving Show

numberOfLiberties (Group p s l) = length l

(<+>) :: ConnectedGroup -> ConnectedGroup -> ConnectedGroup
(Group p1 s1 l1) <+> (Group p2 s2 l2) | p1 == p2 = Group p1 combinedStones ((l1 `union` l2) \\ combinedStones) 
                                                     where 
                                                       combinedStones = (s1 `union` s2)

isOnGrid::Point -> Board -> Bool
isOnGrid (Point x y) (Board s _) = (1 <= x) && (x < s) && (1 <= y) && (y < s) 

placeStone::Player -> Point -> Board -> Board
placeStone pl p b =  undefined --neighbourhood p