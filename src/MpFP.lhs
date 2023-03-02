\documentclass{article}
\begin{document}
\begin{code}
module MpFP 
    ( -- * Data type
      Person             -- TASK 2
    , Destination
      -- * Functions
    , squarePositive     -- TASK 1
    , jointDestinations  -- TASK 2
    , numberChain        -- TASK 3
    , totalDistance      -- TASK 4
    ) where

import qualified Data.List as List

\end{code}
\section{En lista med heltal}

\begin{code}
-- | Square all positive integers in a list.
-- Ignore and exclude both negative numbers and zero from the list.
squarePositive :: [Int] -> [Int]
squarePositive = map (^2) . filter (>0) 

\end{code}

\section{Resmål}
\begin{code}

type Destination = [Char]

data Person = Person
    { name         :: [Char] 
    , age          :: Int
    , destinations :: [Destination]
    } deriving (Show)

-- TODO: Beskriv "point-free style" och "Function Composition"
-- Kanske står beskrivet i en mattebok (diskret) eller Chalmers Haskell ppt eller liknande
jointDestinations :: [Person] -> [Destination]
jointDestinations = map head . List.group . List.sort . concatMap destinations

\end{code}

\section{Svansrekursiv sifferkedja}

\begin{code}

numberChain :: Int -> Int -> [Int]
numberChain x y = reverse $ numberChain' (x:[]) y
  where
    numberChain' :: [Int] -> Int -> [Int]
    numberChain' ys@(x:xs) y 
      | x == y    = ys
      | x < y     = numberChain' (x+2:ys) y
      | otherwise = numberChain' (x-3:ys) y
                    
\end{code}

\section{Kortaste vägen}
\begin{code}
type Point    = (Double, Double)
type Distance = Double

square :: Num a => a -> a
square x = x * x

-- distance :: Floating a => (a, a) -> (a, a) -> a
distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) = sqrt $ square x' + square y'
 where
    x' = abs $ x2 - x1
    y' = abs $ y2 - y1

-- totalDistance :: Floating a => [(a, a)] -> a
totalDistance :: [Point] -> Distance
totalDistance [] = 0
totalDistance ps@(_:ps') = sum $ zipWith distance ps ps'
\end{code}
\end{document}
