-- | First assignment for IB016, semester spring 2016
--
-- You can use any modules from the @Base@ package if you wish.
-- However, try not to use list indexing,
-- i.e. do not use the function '!!'.
--
-- Name: Maros Seleng
-- UID: 422624

module Matrix (
    -- * Matrix type
      Matrix (..)
    -- * Printing
    , pprint
    -- * Properties
    , valid
    , dimensions
    , square
    -- * Manipulation and identity
    , diagonal
    , transpose
    , identity
    -- * Numerical operations
    , scalarMultiply
    , add
    , subtract'
    , multiply
    -- * Bonus
    , determinant
    ) where

import qualified Data.List as List

-- | Matrix is represented as a list of rows of elements.
newtype Matrix a = Matrix { unMatrix :: [[a]] } deriving ( Show, Eq )

-- | Check if 'Matrix' is valid, it is valid if all rows have the same size.
--
-- >>> valid (Matrix [[1,2,2], [3,0,4]])
-- True
--
-- >>> valid (Matrix [[1,2], [3,4,0]])
-- False
valid :: Matrix a -> Bool
valid m = empty m ||
          all (== head columnsInAllRows) columnsInAllRows
            where columnsInAllRows = map length (unMatrix m)

-- | Check if the given 'valid' 'Matrix' is a square matrix.
--
-- >>> square (Matrix [[1,2,2], [3,0,4]])
-- False
--
-- >>> square (Matrix [[1,2], [3,0]])
-- True
square :: Matrix a -> Bool
square m = empty m ||
           all (== length (unMatrix m)) columnsInEveryRow
             where columnsInEveryRow = map length (unMatrix m)

-- | Return dimensions (number of rows, length of rows) of a 'valid' 'Matrix'.
--
-- >>> dimensions (Matrix [[1,2,2], [3,0,4]])
-- (2, 3)
dimensions :: Matrix a -> (Int, Int)
dimensions m
          | empty m   = (0,0)
          | otherwise = (length $ unMatrix m, width)
              where width = head $ map length (unMatrix m)

-- | Return a diagonal of a given 'valid' 'Matrix' if it is 'square' matrix, or
-- 'Nothing' otherwise.
--
-- >>> diagonal (Matrix [[1,2,2], [0,3,4], [0,0,0]])
-- Just [1, 3, 0]
--
-- >>> diagonal (Matrix [[1,2,2], [3,0,4]])
-- Nothing
diagonal :: Matrix a -> Maybe [a]
diagonal m
        | (not . square) m = Nothing
        | empty m          = Just []
        | otherwise        = Just (diagonalFromList $ unMatrix m)

diagonalFromList :: [[a]] -> [a]
diagonalFromList []          = []
diagonalFromList ([]:_)      = [] -- just for the pattern to be exhaustive.
diagonalFromList ((x:_):xss) = x : diagonalFromList [t | (_:t) <- xss]


-- | Transpose a 'valid' matrix.
--
-- >>> transpose (Matrix [[1,2], [3,4]])
-- Matrix [[1,3], [2,4]]
--
-- >>> transpose (identity 3) == identity 3
-- True
transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix $ List.transpose m

-- | For given dimension, return a square identity 'Matrix'.
--
-- >>> identity 4
-- Matrix [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
identity :: Num a => Int -> Matrix a
identity n = Matrix [row | y <- [1..n],
                     let row = [number | x <- [1..n],
                                let number = if y == x then 1 else 0]]

-- | Multiply a 'Matrix' with a scalar. Matrices are expected to be 'valid'.
--
-- >>> scalarMultiply 3 (Matrix [[1,2], [3,4]])
-- Matrix [[3,6], [9,12]]
scalarMultiply :: Num a => a -> Matrix a -> Matrix a
scalarMultiply number matrix = Matrix (map (map (* number)) $ unMatrix matrix)

-- | Add two matrices if they can be added, return 'Nothing' otherwise.
-- Matrices are expected to be 'valid'.
--
-- >>> add (Matrix [[1,2], [3,4]]) (Matrix [[1,0], [0,1]])
-- Just (Matrix [[2,2], [3,5]])
--
-- >>> add (Matrix [[1,2], [3,4]]) (Matrix [])
-- Nothing
add :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
add x y
   | dimensions x /= dimensions y = Nothing
   | otherwise                    = Just (Matrix sol)
       where sol = zipWith (zipWith (+)) (unMatrix x) (unMatrix y)

-- | Subtract two matrices if possible, return 'Nothing' otherwise.
-- Matrices are expected to be 'valid'.
--
-- The prime in the function's name avoids a clash with the Prelude's 'subtract'.
--
-- >>> subtract' (Matrix [[1,2], [3,4]]) (Matrix [[1,0], [0,1]])
-- Just (Matrix [[0,2], [3,3]])
--
-- >>> subtract' (Matrix [[1,2], [3,4]]) (Matrix [])
-- Nothing
subtract' :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
subtract' x y
         | dimensions x /= dimensions y = Nothing
         | otherwise                    = Just (Matrix sol)
             where sol = zipWith (zipWith (-)) (unMatrix x) (unMatrix y)

-- | Multiply two matrices if they can be multipled, returns 'Nothing'
-- otherwise. Matrices are expected to be 'valid'.
--
-- >>> multiply (Matrix [[1,2], [3,4]]) (Matrix [[2,0], [1,1]])
-- Matrix [[4,2], [10,4]]
multiply :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
multiply m n
        | snd (dimensions m) /= fst (dimensions n) = Nothing
        | otherwise                                = Just (Matrix res)
            where res = [row | rowM <- rows m,
                         let row = [element | colN <- cols n,
                                    let element = sum $ zipWith (*) rowM colN]]

-- | Pretty-print a 'Matrix'. All columns should have same width and should be
-- aligned to the right.
--
-- >>> putStrLn . pprint $ identity 3
-- 1 0 0
-- 0 1 0
-- 0 0 1
--
-- >>>  putStrLn . pprint $ Matrix [[1, 42, 128], [0, 1, 2]]
--   1  42 128
--   0   1   2
pprint :: Show a => Matrix a -> String
pprint (Matrix m) = unlines $ map (printRow colWidth) m
                      where colWidth = maximum $ map (length . show) (concat m)

-- Prints the row of a matrix with the specified width of a column.
-- Function truncates elements which are longer than the desired column width.
--
-- This (truncating) should NEVER occur when used in 'pprint' function.
--
-- >>> printRow 4 [1,23,456,7890]
-- "   1   23  456 7890"
--
-- >>> printRow 2 [1,23,456,7890]
-- " 1 23 45 78"
printRow :: Show a => Int -> [a] -> String
printRow _ [] = []
printRow n xs = unwords $ map (showToCertainWidth n) xs

-- | Prints the element to desired number of characters.
-- If the desired length is less than the actual size,
-- the characters will be truncated!
--
-- However this (truncating) should NOT happen while using 'pprint' function.
--
-- >>> showToCertainWidth 5 6
-- "    6"
--
-- >>> showToCertainWidth 3 1024
-- "102"
showToCertainWidth :: Show a => Int -> a -> String
showToCertainWidth l s
                  | l <= length argString = take l argString
                  | otherwise             = prependSpaces spacesNeeded argString
                      where argString    = show s
                            spacesNeeded = l - length argString

-- | Add desired number of spaces to the left side of the second argument.
--
-- >>> prependSpaces 4 "a"
-- "    a"
prependSpaces :: Int -> String -> String
prependSpaces 0 s = s
prependSpaces n s = ' ' : prependSpaces (n-1) s

-- | Compute the determinant of a given 'Matrix'.
-- The input 'Matrix' is expected to be 'valid' and 'square'.
--
-- For simplicity, use the
-- <https://en.wikipedia.org/wiki/Laplace_expansion Laplace expansion> method.
--
-- Implementing this function is optional and awords bonus points for the task.
--
-- >>> determinant (Matrix [[1,5,2], [3,4,0], [0,2,0]])
-- 12
determinant :: Num a => Matrix a -> a
determinant m
           | empty m   = 0
           | otherwise = listDeterminant $ unMatrix m

-- | Computes determinant on list of lists.
-- The given list is expected to be from the 'valid' and 'square' 'Matrix'
--
-- >>> listDeterminant [[1,5,2], [3,4,0], [0,2,0]]
-- 12
listDeterminant :: Num a => [[a]] -> a
listDeterminant []     = 0
listDeterminant [[x]]  = x
listDeterminant (x:xs) = sum $ zipWith (*) firstRowWithSigns determinantsOfSublists
                           where firstRowWithSigns = zipWith (*) [(-1) ^ n | n <- [firstExponent..]] x
                                 firstExponent = if odd $ length x then 0 else length x
                                 colsFromTheRest = List.transpose xs
                                 subcolsAfterDropping = [c | n <- [0..length x - 1], let c = dropNthSublist n colsFromTheRest]
                                 determinantsOfSublists = map listDeterminant subcolsAfterDropping

-- | Returns the list of lists without the nth list. List is 0-indexed
-- When given an empty list, returns empty list.
--
-- >>> dropNthSublist 1 [[0,0,0],[1,1,1],[2,2,2]]
-- [[0,0,0],[2,2,2]]
--
-- >>> dropNthSublist 3 [[0,0,0],[1,1,1],[2,2,2]]
-- [[0,0,0],[1,1,1],[2,2,2]]
--
-- >>> dropNthSublist 1 []
-- []
dropNthSublist :: Int -> [[a]] -> [[a]]
dropNthSublist _ []     = []
dropNthSublist 0 (_:xs) = xs
dropNthSublist n (x:xs) = x : dropNthSublist (n-1) xs

-- | Helper function that checks whether the given 'valid' 'Matrix' is empty.
--
-- >>> empty (Matrix [])
-- True
--
-- >>> empty (Matrix [[],[],[],[]])
-- True
--
-- >>> empty (Matrix [[1,2,3],[2,3,4],[3,4,5]])
-- False
empty :: Matrix a -> Bool
empty (Matrix []) = True
empty (Matrix m)  = let heights = map length m in
                        all (==0) heights

-- | Returns list of rows of a 'Matrix'
-- The input 'Matrix' is expected to be 'valid'.
--
-- >>> rows (Matrix [[1,2],[3,4]])
-- [[1,2],[3,4]]
rows :: Matrix a -> [[a]]
rows = unMatrix

-- | Returns list of columns of a 'Matrix'
-- The input 'Matrix' is expected to be 'valid'.
--
-- >>> rows (Matrix [[1,2],[3,4]])
-- [[1,3],[2,4]]
cols :: Matrix a -> [[a]]
cols = rows . transpose
