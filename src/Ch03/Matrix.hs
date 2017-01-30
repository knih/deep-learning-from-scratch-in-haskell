module Ch03.Matrix where

import Prelude hiding (and, or)
import Numeric.LinearAlgebra

import Control.Exception

-- Matrix
as :: Matrix R
as = matrix 4 [1,2,3,4]
--  (1><4)
-- [ 1.0, 2.0, 3.0, 4.0 ]

as' :: Vector R
as' = vector [1,2,3,4]
-- [1.0,2.0,3.0,4.0]

bs :: Matrix R
bs = (3><2) [1..6]
-- (3><2)
--  [ 1.0, 2.0
--  , 3.0, 4.0
--  , 5.0, 6.0 ]

-- Dimensions
ndim  :: Matrix t -> Int
ndim     = rows

shape :: Matrix t -> (Int, Int)
shape xs = (rows xs, cols xs)

t1 = assert (shape as == (1,4)) "OK"
t2 = assert (shape bs == (3,2)) "OK"

-- Inner Product
as1 :: Matrix R
as1 = (2><2) [1..4]
bs1 :: Matrix R
bs1 = (2><2) [5..8]

as1bs1 :: Matrix R
as1bs1 = as1 <> bs1
-- (2><2)
--  [ 19.0, 22.0
--  , 43.0, 50.0 ]

as2 :: Matrix R
as2 = (2><3) [1..6]
bs2 :: Matrix R
bs2 = (3><2) [1..6]

as2bs2 :: Matrix R
as2bs2 = as2 <> bs2
-- (2><2)
--  [ 22.0, 28.0
--  , 49.0, 64.0 ]

c :: Matrix R
c = (2><2) [1..4]

-- Inconsistent Dimensions
as2c :: Matrix R
as2c = as2 <> c
-- *** Exception: inconsistent dimensions in matrix product (2,3) x (2,2)
-- CallStack (from HasCallStack):
--   error, called at src/Internal/LAPACK.hs:56:31 in hmatrix-0.17.0.2-HoBfYSJ1dMG9PoVkmbcnW2:Internal.LAPACK
