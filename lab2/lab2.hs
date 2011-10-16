module Main where

import Data.Bits
import Data.List (partition, sort)
import Hilbert

data Point = Point { xCoord :: Int, yCoord :: Int } deriving (Eq, Show)
data Rectangle = Rectangle {
        p1 :: Point,
        p2 :: Point,
        hv :: Int
        } deriving (Eq, Show)

hilbertOrder :: Int
hilbertOrder = 16

mkRectangle :: Point -> Point -> Rectangle
mkRectangle p1 p2 = Rectangle p1 p2 hv
        where hv = hilbertDistance hilbertOrder $ (xCoord c, yCoord c)
              c = Point xm ym
              xm = div (xCoord p1 + xCoord p2) 2
              ym = div (yCoord p1 + yCoord p2) 2

instance Ord Rectangle where
        compare r1 r2 = compare (hv r1) (hv r2)

minimumBoundingRect :: [Rectangle] -> Rectangle
minimumBoundingRect rects = mkRectangle (Point xMin yMin) (Point xMax yMax)
        where xMin = minimum $ map (xCoord . p1) rects
              xMax = maximum $ map (xCoord . p2) rects
              yMin = minimum $ map (yCoord . p1) rects
              yMax = maximum $ map (yCoord . p2) rects



leafCapacity :: Int
leafCapacity = 3

nonLeafCapacity :: Int
nonLeafCapacity = 4

data NodeContent = NonLeafC Rectangle Int Forest | LeafC [Rectangle] deriving Show
data RTree = Tip | Node NodeContent deriving Show
type Forest = [RTree]

intersect :: Rectangle -> Rectangle -> Bool
intersect r1 r2 = not (
           r2_left > r1_right
        || r2_right < r1_left
        || r2_top < r1_bottom
        || r2_bottom > r1_top
        ) where r2_left   = xCoord $ p1 r2
                r1_right  = xCoord $ p2 r1
                r2_right  = xCoord $ p2 r2
                r1_left   = xCoord $ p1 r1
                r2_top    = yCoord $ p2 r2
                r1_bottom = yCoord $ p1 r1
                r2_bottom = yCoord $ p1 r2
                r1_top    = yCoord $ p2 r1


insert :: Rectangle -> RTree -> RTree
insert r Tip = Node content
        where content = LeafC [r]
insert r (Node content) = case content of
                               LeafC rects -> Node $ LeafC $ sort (r:rects)
                               NonLeafC r mhv forest -> error "non-leaf"





--  data LeafItem = LeafItem { rect :: Rectangle, hv :: Int } deriving Show
--  data RTree = EmptyRTree
--               | Node { mbr :: Rectangle, mhv :: Int, nodeCapacity :: Int, children :: [RTree] }
--               | Leaf { leafCapacity :: Int, items :: [LeafItem] }
--               deriving Show

--  isLeafFull :: RTree -> Bool
--  isLeafFull (Leaf c is) = c <= length is
--  isLeafFull _ = error "called isLeafFull on non-leaf"

--  mkLeafItem :: Rectangle -> LeafItem
--  mkLeafItem r = LeafItem r $ distance r

--  singleton :: RTree
--  singleton = EmptyRTree

--  insertRectInLeaf :: Rectangle -> RTree -> RTree
--  insertRectInLeaf r (Leaf c is) = Leaf c $ iinf ++ [newLeaf] ++ isup
--          where newLeaf = mkLeafItem r
--                (iinf, isup) = partition (\x -> (hv x) < (hv newLeaf)) is

--  insert :: Rectangle -> RTree -> RTree
--  insert r EmptyRTree = Leaf 10 $ [mkLeafItem r] -- TODO leaf capacity param of RTree
--  insert r l@(Leaf c is)
--          | not (isLeafFull l) = insertRectInLeaf r l
--          | otherwise = error "TODO overflow"
--  insert r (Node br h c cs) = error "TODO insert node"

--  chooseLeaf :: RTree -> Rectangle -> RTree
--  chooseLeaf l@(Leaf _ _) _ = l
--  chooseLeaf t r = error "TODO"

--  search :: RTree -> Rectangle -> [Rectangle]
--  search EmptyRTree _ = []
--  search (Leaf c is) r = filter (intersect r) (map rect is)
--  search (Node br _ _ cs) r
--          | intersect r br = foldr (++) [] $ map (\t -> search t r) cs
--          | otherwise = []


main = do
        let r0 = mkRectangle (Point 100 100) (Point 110 110)
        let r1 = mkRectangle (Point 0 0) (Point 10 10)
        let r2 = mkRectangle (Point 0 0) (Point 8 12)
        let r3 = mkRectangle (Point 0 0) (Point 1 2)
        --  putStrLn $ show r1
        --  putStrLn $ show $ sort [r0, r1, r2, r3]
        let l1 = Node $ LeafC [r0, r1, r2]
        putStrLn $ show l1


