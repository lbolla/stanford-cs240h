module Main where

import Data.Bits
import Data.List (partition)
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

isLeafFull :: RTree -> Bool
isLeafFull (Leaf c is) = c <= length is
isLeafFull _ = error "called isLeafFull on non-leaf"

mkLeafItem :: Rectangle -> LeafItem
mkLeafItem r = LeafItem r $ distance r

singleton :: RTree
singleton = EmptyRTree

insertRectInLeaf :: Rectangle -> RTree -> RTree
insertRectInLeaf r (Leaf c is) = Leaf c $ iinf ++ [newLeaf] ++ isup
	where newLeaf = mkLeafItem r
	      (iinf, isup) = partition (\x -> (hv x) < (hv newLeaf)) is

insert :: Rectangle -> RTree -> RTree
insert r EmptyRTree = Leaf 10 $ [mkLeafItem r] -- TODO leaf capacity param of RTree
insert r l@(Leaf c is)
	| not (isLeafFull l) = insertRectInLeaf r l
	| otherwise = error "TODO overflow"
insert r (Node br h c cs) = error "TODO insert node"

chooseLeaf :: RTree -> Rectangle -> RTree
chooseLeaf l@(Leaf _ _) _ = l
chooseLeaf t r = error "TODO"

search :: RTree -> Rectangle -> [Rectangle]
search EmptyRTree _ = []
search (Leaf c is) r = filter (intersect r) (map rect is)
search (Node br _ _ cs) r
	| intersect r br = foldr (++) [] $ map (\t -> search t r) cs
	| otherwise = []

intersect :: Rectangle -> Rectangle -> Bool
intersect r1 r2 = not ( 
		   r2_left > r1_right
		|| r2_right < r1_left
		|| r2_top < r1_bottom
		|| r2_bottom > r1_top
		)
	where r2_left   = xCoord $ p1 r2
	      r1_right  = xCoord $ p2 r1
	      r2_right  = xCoord $ p2 r2
	      r1_left   = xCoord $ p1 r1
	      r2_top    = yCoord $ p2 r2
	      r1_bottom = yCoord $ p1 r1
	      r2_bottom = yCoord $ p1 r2
	      r1_top    = yCoord $ p2 r1

main = do
	let r0 = Rectangle (Point 100 100) (Point 110 110)
	let r1 = Rectangle (Point 0 0) (Point 10 10)
	let r2 = Rectangle (Point 0 0) (Point 8 12)
	let r3 = Rectangle (Point 0 0) (Point 1 2)
	--  putStrLn $ show $ center r1
	--  putStrLn $ show $ distance r1
	--  putStrLn $ show $ r1 == r2
	--  putStrLn $ show $ r1 > r2
	--  putStrLn $ show $ minimumBoundingRect [r1, r2]
	--  let l1 = mkLeafItem r1
	--  putStrLn $ show l1
	--  let t1 = mkLeaf 10 [r1, r2]
	--  putStrLn $ show t1
	--  let t1 = insert r1 singleton
	--  putStrLn $ show $ t1
	--  let t2 = insert r2 t1
	--  putStrLn $ show $ t2
	--  let t3 = insert r3 t2
	--  putStrLn $ show $ t3
	--  let t0 = Node r1 (distance r1) 10 [Leaf 10 [mkLeafItem r1]]

	let l1 = Leaf 10 [
			mkLeafItem r1,
			mkLeafItem r2
		]
	let l2 = Leaf 10 [
			mkLeafItem r1,
			mkLeafItem r3
		]
	let t0 = Node r1 (distance r1) 10 [l1, l2]
	putStrLn $ show $ t0
	
	putStrLn $ show $ search EmptyRTree r1
	putStrLn $ show $ search l1 r1
	putStrLn $ show $ search t0 r1
	putStrLn $ show $ search t0 r0
