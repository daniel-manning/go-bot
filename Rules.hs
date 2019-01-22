--import Data.Set
import Data.List
import Data.Maybe

data Player = Black | White deriving (Show, Eq)

data Point = Point Int Int deriving (Show, Eq)

data Move = Play Point | Pass | Resign deriving Show

data Board = Board Int [ConnectedGroup]

neighbourhood::Point -> [Point]
neighbourhood (Point x y) = [Point (x - 1) y, Point x (y + 1), Point x (y - 1), Point (x + 1) y]

data ConnectedGroup = Group Player [Point] [Point] deriving Show

numberOfLiberties (Group p s l) = length l

(<+>) :: ConnectedGroup -> ConnectedGroup -> ConnectedGroup
(Group p1 s1 l1) <+> (Group p2 s2 l2) | p1 == p2 = Group p1 combinedStones ((l1 `union` l2) \\ combinedStones) 
                                                     where 
                                                       combinedStones = (s1 `union` s2)

isOnGrid::Point -> Board -> Bool
isOnGrid (Point x y) (Board s _) = (1 <= x) && (x < s) && (1 <= y) && (y < s) 

groupContainsPoint::Point -> ConnectedGroup -> Bool
groupContainsPoint p (Group pl st _) = p `elem` st

whichPlayer::ConnectedGroup -> Player
whichPlayer (Group pl _ _) = pl

getSpaceOnGrid::Point -> Board -> Maybe Player
getSpaceOnGrid p (Board _ []) = Nothing
getSpaceOnGrid p (Board n (cg:cgs)) | groupContainsPoint p cg  = Just (whichPlayer cg)
                                    | otherwise = getSpaceOnGrid p (Board n (cgs))
                                            
buildConnectedGroupFromNeighbourhood::[Point] -> ConnectedGroup
buildConnectedGroupFromNeighbourhood p =  undefined $ map (\k -> (k, getSpaceOnGrid k)) p

f::Player -> [(Point, Maybe Player)] -> ConnectedGroup
f pl p = Group pl [undefined] (map fst $ filter (isNothing.snd) p)


placeStone::Player -> Point -> Board -> Board
placeStone pl p b = undefined --filter(\p1 -> isOnGrid p1 b) $ neighbourhood p


