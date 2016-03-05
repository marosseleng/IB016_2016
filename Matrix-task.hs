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
valid (Matrix []) = True
valid (Matrix m)  = let columnsInRow = map length m in
                      null columnsInRow ||
                      all (== head columnsInRow) columnsInRow

-- | Check if the given 'valid' 'Matrix' is a square matrix.
--
-- >>> square (Matrix [[1,2,2], [3,0,4]])
-- False
--
-- >>> square (Matrix [[1,2], [3,0]])
-- True
square :: Matrix a -> Bool
square (Matrix []) = True
square (Matrix m)  = let columnsInRow = map length m in
                       all (== length m) columnsInRow ||
                       all (== 0) columnsInRow

-- | Return dimensions (number of rows, length of rows) of a 'valid' 'Matrix'.
--
-- >>> dimensions (Matrix [[1,2,2], [3,0,4]])
-- (2, 3)
dimensions :: Matrix a -> (Int, Int)
dimensions (Matrix []) = (0,0)
dimensions (Matrix m) = let height = head $ map length m in
                          if height == 0
                            then (0, height)
                            else (length m, height)

-- | Return a diagonal of a given 'valid' 'Matrix' if it is 'square' matrix, or
-- 'Nothing' otherwise.
--
-- >>> diagonal (Matrix [[1,2,2], [0,3,4], [0,0,0]])
-- Just [1, 3, 0]
--
-- >>> diagonal (Matrix [[1,2,2], [3,0,4]])
-- Nothing
diagonal :: Matrix a -> Maybe [a]
diagonal = undefined

-- | Transpose a 'valid' matrix.
--
-- >>> transpose (Matrix [[1,2], [3,4]])
-- Matrix [[1,3], [2,4]]
--
-- >>> transpose (identity 3) == identity 3
-- True
transpose :: Matrix a -> Matrix a
transpose = undefined

-- | For given dimension, return a square identity 'Matrix'.
--
-- >>> identity 4
-- Matrix [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
identity :: Num a => Int -> Matrix a
identity = undefined

-- | Multiply a 'Matrix' with a scalar. Matrices are expected to be 'valid'.
--
-- >>> scalarMultiply 3 (Matrix [[1,2], [3,4]])
-- Just (Matrix [[3,6], [9,12]])
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
add x y = if (dimensions x) /= (dimensions y) 
            then Nothing
            else Just (Matrix (zipWith (zipWith (+)) (unMatrix x) (unMatrix y)))
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
subtract' x y = if (dimensions x) /= (dimensions y)
                  then Nothing
                  else Just (Matrix (zipWith (zipWith (-)) (unMatrix x) (unMatrix y)))

-- | Multiply two matrices if they can be multipled, returns 'Nothing'
-- otherwise. Matrices are expected to be 'valid'.
--
-- >>> multiply (Matrix [[1,2], [3,4]]) (Matrix [[2,0], [1,1]])
-- Matrix [[4,2], [10,4]]
multiply :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
multiply = undefined

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
pprint = undefined

-- | Compute the determinant of a given 'Matrix'.
-- The input 'Matrix' is expected to be 'valid'.
--
-- For simplicity, use the
-- <https://en.wikipedia.org/wiki/Laplace_expansion Laplace expansion> method.
--
-- Implementing this function is optional and awords bonus points for the task.
--
-- >>> determinant (Matrix [[1,5,2], [3,4,0], [0,2,0]])
-- 12
determinant :: Num a => Matrix a -> a
determinant = undefined

