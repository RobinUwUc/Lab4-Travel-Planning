module SkewHeap (
    SkewHeap(..),
    empty',
    isEmpty,
    insert,
    merge,
    delete,
    deleteIfFound,
    findMin,
    toList
) where

data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)

-- | Create an empty skew heap.
-- This operation is O(1).
empty' :: SkewHeap a
empty' = Empty

isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Convert a skew heap to a list.
-- The list is sorted in ascending order.
-- This operation is O(n log n) in the size of the heap.
toList :: Ord a => SkewHeap a -> [a]
toList heap = case findMin heap of
    Nothing -> []
    Just x  -> x : toList (delete x heap)
  
-- | Merge two skew heaps.
-- This operation is O(log n) in the size of the heaps.
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h Empty = h
merge Empty h = h
merge h1@(Node x l1 r1) h2@(Node y l2 r2)
    | x <= y    = Node x (merge r1 h2) l1
    | otherwise = Node y (merge r2 h1) l2

-- | Insert an element into the skew heap.
-- The element is added as a new node, and the heap is merged with the
-- existing heap.
-- This operation is O(1) because it only requires looking at the root node.
insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x = merge (Node x Empty Empty)

-- | Find the minimum element in the skew heap.
-- This is a constant time operation.
findMin :: SkewHeap a -> Maybe a
findMin Empty = Nothing
findMin (Node x _ _) = Just x

-- this takes O(log n) time for each delete, where n is the number of elements in the heap.
-- The overall time complexity is O(m log n), where m is the number of elements to delete.
-- | Delete a single element from the skew heap.
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete _ Empty = Empty 
delete val (Node x l r)
    | x == val = merge l r
    | otherwise = 
        let nextLeft = delete val l
        in if nextLeft /= l
            then Node x nextLeft r
            else Node x l (delete val r)

--This takes O(log n) time for each delete, where n is the number of elements in the heap.
-- The overall time complexity is O(m log n), where m is the number of elements to delete.
-- Delete a single element from the skew heap if it is found and return a bool indicating if it was found
deleteIfFound :: Ord a => a -> SkewHeap a -> (Bool, SkewHeap a)
deleteIfFound _ Empty = (False, Empty) 
deleteIfFound val (Node x l r)
    | x == val = (True, merge l r)
    | otherwise = 
        let (foundL, l') = deleteIfFound val l in
        if foundL
            then (True, Node x l' r)
            else 
                let (foundR, r') = deleteIfFound val r in
                (foundR, Node x l r')

