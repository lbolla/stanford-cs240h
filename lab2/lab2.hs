module Main where

import Data.Bits
import Hilbert

data Point = Point { xCoord :: Int, yCoord :: Int } deriving (Eq, Show)
data Rectangle = Rectangle {
	p1 :: Point,
	p2 :: Point
} deriving (Eq, Show)

center :: Rectangle -> Point
center (Rectangle p1 p2) = Point xm ym
	where xm = div (xCoord p1 + xCoord p2) 2
	      ym = div (yCoord p1 + yCoord p2) 2

distance :: Rectangle -> Int
distance r = hilbertDistance 16 $ (xCoord c, yCoord c)
	where c = center r

instance Ord Rectangle where
	compare r1 r2 = compare (distance r1) (distance r2)

-- Minimum Bounding Rectangle
minimumBoundingRect :: [Rectangle] -> Rectangle
minimumBoundingRect rects = Rectangle (Point xMin yMin) (Point xMax yMax)
	where xMin = minimum $ map (xCoord . p1) rects
	      xMax = maximum $ map (xCoord . p2) rects
	      yMin = minimum $ map (yCoord . p1) rects
	      yMax = maximum $ map (yCoord . p2) rects

data LeafItem = LeafItem { rect :: Rectangle, hv :: Int } deriving Show
data RTree = EmptyRTree
	     | Node { mbr :: Rectangle, mhv :: Int, nodeCapacity :: Int, children :: [RTree] }
	     | Leaf { leafCapacity :: Int, items :: [LeafItem] }
	     deriving Show

mkLeafItem :: Rectangle -> LeafItem
mkLeafItem r = LeafItem r (distance r)

mkLeaf :: Int -> [Rectangle] -> RTree
mkLeaf c rects = Leaf c $ map mkLeafItem rects

main = do
	let r1 = Rectangle (Point 0 0) (Point 10 10)
	let r2 = Rectangle (Point 0 0) (Point 8 12)
	putStrLn $ show $ center r1
	putStrLn $ show $ distance r1
	putStrLn $ show $ r1 == r2
	putStrLn $ show $ r1 > r2
	putStrLn $ show $ minimumBoundingRect [r1, r2]
	let l1 = mkLeafItem r1
	putStrLn $ show l1
	let t1 = mkLeaf 10 [r1, r2]
	putStrLn $ show t1
