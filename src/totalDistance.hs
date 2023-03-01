{- |
File name      : totalDistance.hs
Module         : main
Description    : Task 4: Kortaste vägen
                 <https://canvas.kth.se/courses/38058/assignments/237975>
Course         : DD1360 VT23 Programmeringsparadigm (progp23)
Author/Student : Vincent Ferrigan
maintainer     : ferrigan@kth.se
-}

-- module Main
module TotalDistance
    ( -- * Functions
      distance
    , totalDistance
    ) where

import qualified Data.List as List

-- type Matrix a = [Row a]
-- type Row a    = [a]
type Point    = (Double, Double)
type Distance = Double

square :: Num a => a -> a
square x = x * x

distance :: Point -> Point -> Distance
-- distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ square x' + square y'
 where
    x' = abs $ x2 - x1
    y' = abs $ y2 - y1

totalDistance :: [Point] -> Distance
totalDistance [] = 0
totalDistance ps@(_:ps') = sum $ zipWith distance ps ps'

-- Fyller ena halvan av matrisen. Inga upprepningar. [_\X] 
distanceMatrix :: [Point] -> [(Point, Point, Distance)]
distanceMatrix [] = []
distanceMatrix ps = [(x, y, distance x y) | ys@(x:xs) <- List.tails ps, y<-ys]

-- Fyller hela matrisen [X'\X]
distanceMatrix' :: [Point] -> [(Point, Point, Distance)]
distanceMatrix' [] = []
distanceMatrix' ps = [(x, y, distance x y) | (x,y) <- (,) <$> ps <*> ps]
-- List.tails returnerar en lista av initiala segment, längst först. 
-- List.tails [1,2,3] -- => [[1,2,3], [2,3], [3], []]
-- TODO Skapa en funktion som skapar en matrix av distanser. Ska distanser vara en datatyp?

-- shortestPath :: [Point] -> Doulbe


-- totalDistance :: [(Int, Int)] -> Double
{- TODO and comments 
 - Floyd–Warshall algorithm?
 - Vi behöver väl en all pair shortest path algorithm?
 - https://www.naukri.com/learning/articles/about-floyd-warshall-algorithm/
-}
