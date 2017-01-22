module Ch02.AND where

import Prelude hiding (and, or)
import Numeric.LinearAlgebra

and_ :: (Ord a, Fractional a, Num b) => a -> a -> b
and_ x1 x2 = if (x1*w1 + x2*w2) <= theta then 0 else 1
  where
    w1    = 0.5
    w2    = 0.5
    theta = 0.7
-- λ> and_ 0 0
-- 0
-- λ> and_ 1 0
-- 0
-- λ> and_ 0 1
-- 0
-- λ> and_ 1 1
-- 1

perceptron :: R -> R -> R -> R -> R -> R
perceptron w1 w2 b = \x1 x2 ->
  if (sum $ toList $ (vector [x1,x2]) * vector [w1,w2]) + b <= 0 then 0 else 1

and       = perceptron   0.5    0.5  (-0.7)
nand      = perceptron (-0.5) (-0.5)   0.7
or        = perceptron   0.5    0.5  (-0.2)
xor x1 x2 = and (nand x1 x2) (or x1 x2)
