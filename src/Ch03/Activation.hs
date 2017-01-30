module Ch03.Activation where

import Prelude hiding (and, or)
import Numeric.LinearAlgebra hiding (step)
import Graphics.Gnuplot.Simple

-- Activation functions
step :: [R] -> [R]
step = map (\x -> if x > 0.0 then 1.0 else 0.0)

sigmoid :: [R] -> [R]
sigmoid = map (\x -> 1 / (1 + exp (-x)))

relu :: [R] -> [R]
relu = map $ max 0

-- Graphs
step_graph :: IO ()
step_graph = plotPath  [Title "step", YRange (-0.2, 1.2)] $ zip xs ys
  where
    xs = linearScale 1000 (-5.0, 5.0)
    ys = step xs

sigmoid_graph :: IO ()
sigmoid_graph = plotPath  [Title "sigmoid", YRange (-0.2, 1.2)] $ zip xs ys
  where
    xs = linearScale 1000 (-5.0, 5.0)
    ys = sigmoid xs

sigmoid_step_graph :: IO ()
sigmoid_step_graph = plotPathsStyle
                     [Title "sigmoid and step", YRange (-0.2, 1.2)]
                     [(style1, zip xs ys1), (style2, zip xs ys2)]
  where
    xs     = linearScale 1000 (-5.0, 5.0)
    ys1    = sigmoid xs
    ys2    = step xs
    style1 = PlotStyle Lines $ CustomStyle [(LineTitle "sigmoid")]
    style2 = PlotStyle Lines $ CustomStyle [(LineTitle "step")]

relu_graph :: IO ()
relu_graph = plotPath [Title "ReLU", YRange (-1.0, 5.5)] $ zip xs ys
  where
    xs = linearScale 1000 (-5.0, 5.0)
    ys = relu xs
