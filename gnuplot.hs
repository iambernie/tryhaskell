import Graphics.Gnuplot.Simple
plotFunc [] (linearScale 1000 (-20,20)) (\x -> sin x / x)
