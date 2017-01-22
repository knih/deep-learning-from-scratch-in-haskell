module Ch01.Plot where

import Graphics.Gnuplot.Simple

sin_graph :: IO ()
sin_graph = plotPath [(Title "hello")] points
  where
    points :: [(Double, Double)]
    points = zip xs ys
    -- xs = [0.0, 0.1 .. 6.0]
    xs = linearScale 100 (0.0, 6.0)
    ys = fmap sin xs

sin_cos_graph :: IO ()
sin_cos_graph = plotPathsStyle attr [(style1, zip xs ys1), (style2, zip xs ys2)]
  where
    attr = [(Title "sin and cos")
           , (XLabel "x")
           , (YLabel "y")
           ]
    xs :: [Double]
    xs  = linearScale 100 (0.0, 6.0)
    ys1 = fmap sin xs
    ys2 = fmap cos xs
    style1 = PlotStyle Lines $ CustomStyle [(LineTitle "sin")]
    style2 = PlotStyle Lines $ CustomStyle [(LineTitle "cos")]
