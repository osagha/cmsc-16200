module Spaces where

import SimplicialComplex
import Data.List

point,line :: SComplex
simplex,ball,sphere :: Int -> SComplex
    
point = [[0]]
line = [[0],[1],[0,1]]

simplex n = faces [0..n]
ball n = sublists [0..n] \\ [[]]
sphere n = ball (n+1) \\ [[0..(n+1)]]