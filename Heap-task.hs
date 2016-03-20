-- | Second assignment for IB016 Seminar on Functional Programming, spring 2016
--
-- Your task is to implement a priority queue based on heap data structure. A
-- priority queue is a data structure which holds values sorted by priority
-- (so that the value with the highest priority is at the top of the heap). The
-- interface is given by this file, together with suggested time complexities
-- for each of the functions. Your implementation should meet these time
-- complexities.
--
-- It is suggested to use binary heap. You can also use more advanced versions
-- of heaps, for example binomial heap or Fibonacci heap. It is possible to
-- meet all the requirements with binary heap which is reasonably simple to
-- implement. You can get extra points for more advanced heap implementations.
--
--   * <https://en.wikipedia.org/wiki/Binary_heap Binary heap>
--   * <https://en.wikipedia.org/wiki/Binomial_heap Binomial heap>
--   * <https://en.wikipedia.org/wiki/Fibonacci_heap Fibonacci heap>
--
-- You may define your own (internal) functions that the module does not export
-- (e.g. helper functions for tree rotations and such).
-- You are not required (but still strongly encouraged) to document such functions
-- using the <https://www.haskell.org/haddock/doc/html/ch03s08.html Haddock markup>.
-- You can use any modules from the @Base@ package if you wish.
-- However, you may not use any other packages.
--
-- Name: Name Surname
-- UID: 123456

module PriorityQueue
    ( -- * Priority Queue Type
      PriorityQueue
      -- * Construction
    , empty
    , singleton
    , fromList
    , insert
    , union
      -- * Querying
    , isEmpty
    , size
    , getTop
    , toDescList
      -- * Modification
    , extractTop
    , extractGetTop
    , replaceTop
    , modifyTop
      -- * Debugging
    , valid
    ) where

-- | Defines a 'PriorityQueue' type. It should implement a (binary) heap. Note
-- that the data constructors of this type are not exported from the module to
-- disallow direct manipulations. (If they should have been exported the module
-- header entry would read @PriorityQueue (..)@).
--
-- Note: You should define 'Show', 'Eq', and 'Functor' instances. 'Show' and
-- 'Eq' should be defined by hand, not derived. Nevertheless, it is useful to
-- derive 'Show' instance during development and change to a custom-defined
-- instance once you trust your implementation.
data PriorityQueue p v = PQEmpty | PQueue Int (p,v) [Tree p v]
                       deriving (Show)

data Tree p v = Node Int (p,v) [Tree p v]
              deriving (Show)
-- | /O(n log n)/. Define instance of 'Show' similar to the one for
-- 'Data.Map.Map'.
--
-- >>>  show empty
-- "fromList []"
--
-- >>> show $ singleton 42 ()
-- "fromList [(42,())]
--
--instance (Show p, Show v, Ord p) => Show (PriorityQueue p v) where
--  show = undefined

-- | /O(n log n)/. Compare two 'PriorityQueue's for equality of their contents.
-- It does not take into account the concrete structure of the given underlying
-- heaps.
instance (Ord p, Eq v) => Eq (PriorityQueue p v) where
  (==) = undefined

-- These functions are not exported from the module, however, they can be
-- useful for directing descend into the appropriate branch of a binary heap in
-- insert. They calculate minimal and maximal length of path from root to leaf
-- in binary heap of given size.
maxPathForSize :: Int -> Int
maxPathForSize 0 = 0
maxPathForSize n = 1 + maxPathForSize (n `div` 2)

minPathForSize :: Int -> Int
minPathForSize n
    | 2 ^ mh - 1 == n = mh
    | otherwise       = mh - 1
  where
    mh = maxPathForSize n

-- | /O(1)/. An empty 'PriorityQueue'.
--
-- >>> empty
-- fromList []
empty :: PriorityQueue p v
empty = PQEmpty

-- | /O(1)/. A 'PriorityQueue' with a single element.
--
-- >>> singleton 42 "hi"
-- fromList [(42,"hi")]
singleton :: Ord p => p -> v -> PriorityQueue p v
singleton p v = PQueue 1 (p,v) [singletonTree p v]

singletonTree :: p -> v -> Tree p v
singletonTree p v = Node 0 (p,v) []

-- | /O(1)/. Is the 'PriorityQueue' empty?
--
-- >>> isEmpty empty
-- True
-- >>> isEmpty $ singleton 42 "hi"
-- False
isEmpty :: PriorityQueue p v -> Bool
isEmpty PQEmpty = True
isEmpty _       = False

-- | /O(1)/. The number of elements in the 'PriorityQueue'.
--
-- >>> size $ singleton 42 "hi"
-- 1
size :: PriorityQueue p v -> Int
size PQEmpty        = 0
size (PQueue s _ _) = s

-- | /O(log n)/. @insert prio val pq@ inserts the element @val@ with the
-- priority @prio@ into the 'PriorityQueue' @pq@.
--
-- >>> insert 42 "hi" empty
-- fromList [(42,"hi")]
--
-- >>> insert 0 "" $ singleton 42 "hi"
-- fromList [(42,"hi"),(0,"")]
insert :: Ord p => p -> v -> PriorityQueue p v -> PriorityQueue p v
insert p v PQEmpty        = singleton p v
insert p v (PQueue s t l) = PQueue (s + 1) newT newL
                              where newT = if p > fst t then (p,v) else t
                                    newL = insertTree (singletonTree p v) l

insertTree :: Ord p => Tree p v -> [Tree p v] -> [Tree p v]
insertTree t []        = [t]
insertTree t ts@(x:xs)
          | rank t < rank x = t:ts
          | otherwise       = insertTree (link t x) xs

rank :: Tree p v -> Int
rank (Node r _ _) = r

link :: Ord p => Tree p v -> Tree p v -> Tree p v
link s@(Node r1 t1 xs) t@(Node _ t2 ys)
    | fst t1 > fst t2 = Node (r1 + 1) t1 (t:xs)
    | otherwise       = Node (r1 + 1) t2 (s:ys)

-- | /O(1)/. Get the element with highest priority from the given 'PriorityQueue'.
-- Returns 'Nothing' if and only if the 'PriorityQueue' is 'empty'.
--
-- >>> getTop $ insert 0 "" $ singleton 42 "hi"
-- Just (42,"hi")
--
-- >>> getTop empty
-- Nothing
getTop :: Ord p => PriorityQueue p v -> Maybe (p, v)
getTop PQEmpty          = Nothing
getTop (PQueue _ top _) = Just top

-- | /O(log n)/. Extract (remove) the element with highest priority from the
-- given 'PriorityQueue'. Returns 'empty' if the 'PriorityQueue' is 'empty'.
--
-- >>> extractTop $ insert 0 "" $ singleton 42 "hi"
-- fromList [(0,"")]
--
-- >>> extractTop empty
-- fromList []
extractTop :: Ord p => PriorityQueue p v -> PriorityQueue p v
extractTop = undefined

-- | /O(log n)/. Get and extract the element with highest priority from the
-- given 'PriorityQueue'. Returns 'Nothing' if and only if the 'PriorityQueue'
-- is 'empty'.
--
-- >>> extractGetTop $ insert 0 "" $ singleton 42 "hi"
-- Just ((42,"hi"),fromList [(0,"")])
--
-- >>> extractGetTop empty
-- Nothing
extractGetTop :: Ord p => PriorityQueue p v -> Maybe ((p, v), PriorityQueue p v)
extractGetTop = undefined

-- | /O(log n)/. Replace the highest priority element with new priority and
-- value.
--
-- >>> replaceTop 2 "a" $ fromList [(1, ""), (3, "")]
-- fromList [(2,"a"),(1,"")]
--
-- >>> replaceTop 0 "a" $ fromList [(1, ""), (3, "")]
-- fromList [(1,""),(0,"a")]
replaceTop :: Ord p => p -> v -> PriorityQueue p v -> PriorityQueue p v
replaceTop = undefined

-- | /O(log n)/. Replace the highest priority element using a function callback
-- which is called with the current priority and value of the top element. If
-- the callback returns 'Nothing' the root is removed, otherwise new values of
-- priority and value are determined from the returned Just value.
--
-- >>> modifyTop (\k v -> Just (k + 2, v)) $ fromList [(1, ""), (2, "")]
-- fromList [(4,""),(1,"")]
--
-- >>> modifyTop (\k v -> Just (k - 2, v)) $ fromList [(1, ""), (2, "")]
-- fromList [(1,""),(0,"")]
modifyTop :: Ord p => (p -> v -> Maybe (p, v)) -> PriorityQueue p v -> PriorityQueue p v
modifyTop = undefined

-- | /O(n log n)/. Build a 'PriorityQueue' from a list of priority – value pairs.
--
-- Note: this can be done in /O(n)/, you can be awarded bonus points if you
-- implement this version.
--
-- >>> fromList [(0, ()), (4, ()), (17, ()), (3, ())]
-- fromList [(17,()),(4,()),(3,()),(0,())]
fromList :: Ord p => [(p, v)] -> PriorityQueue p v
fromList = undefined

-- | /O(n log n)/. Convert the given 'PriorityQueue' to a list of its priority
-- – value pairs ordered by priority in descending order.
--
-- >>> toDescList $ fromList [(0, ()), (4, ()), (17, ()), (3, ())]
-- [(17,()),(4,()),(3,()),(0,())]
toDescList :: Ord p => PriorityQueue p v -> [(p, v)]
toDescList = undefined

-- | /O(n log n)/. Compute union of two 'PriorityQueue's.
--
-- Note: this can be done in /O(n)/, you can be awarded bonus points if you
-- implement this version.
--
-- >>> fromList [(0, ()), (4, ()), (17, ()), (3, ())] `union` singleton 42 ()
-- fromList [(42,()),(17,()),(4,()),(3,()),(0,())]
union :: Ord p => PriorityQueue p v -> PriorityQueue p v -> PriorityQueue p v
union PQEmpty q = q
union q PQEmpty = q
union (PQueue s1 t1 l1) (PQueue s2 t2 l2) = PQueue newS newT newL
                                              where newS = s1 + s2
                                                    newT = if fst t1 > fst t2 then t1 else t2
                                                    newL = unionOfLists l1 l2

unionOfLists :: Ord p => [Tree p v] -> [Tree p v] -> [Tree p v]
unionOfLists [] l              = l
unionOfLists l []              = l
unionOfLists l@(x:xs) k@(y:ys)
            | rank x > rank y = x : unionOfLists xs k
            | rank y > rank x = y : unionOfLists l ys
            | otherwise       = insertTree (link x y) (unionOfLists xs ys)

-- | Define @'PriorityQueue' p@ to be an instance of 'Functor', keep in mind the
-- standard behaviour of 'Functor'.
instance Functor (PriorityQueue p) where
    fmap = undefined

-- | Check validity of a given 'PriorityQueue', this should validate that 'size'
-- returns the right value for the heap and all its subheaps and that all the
-- requirements given by the data structure are met (e.g. for binary heap,
-- it should check that the tree is left aligned, lengths of paths from root to
-- leaf differ at most by one, and that the priority is nonascending on every
-- path from root to leaf). You should use this function for testing.
valid :: Show p => Ord p => PriorityQueue p v -> Bool
valid = undefined
