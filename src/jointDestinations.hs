{- |
File name      : jointDestinations.hs
Module         : main
Description    : Task 2: Resmål
                 <https://canvas.kth.se/courses/38058/assignments/237975>
Course         : DD1360 VT23 Programmeringsparadigm (progp23)
Author/Student : Vincent Ferrigan
maintainer     : ferrigan@kth.se
-}

-- module Main
module JointDestinations
    ( -- * Data type
      Person
      -- * Functions
    , jointDestinations
    ) where

import Data.List

type Destination = [Char]

data Person = Person
    { name         :: [Char] 
    , age          :: Int
    , destinations :: [Destination]
    } deriving (Show)

-- Alt 1
-- TODO: Beskriv "point-free style" och "Function Composition"
-- Kanske står beskrivet i en mattebok (diskret) eller Chalmers Haskell ppt eller liknande
jointDestinations :: [Person] -> [Destination]
jointDestinations = map head . group . sort . concatMap destinations

-- Alt 2
-- jointDestinations :: [Person] -> [Destination]
-- jointDestinations = jointDestinations' . map destinations

-- jointDestinations' :: [[Destination]] -> [Destination]
-- jointDestinations' []       = []
-- jointDestinations' [x]      = []
-- jointDestinations' [x,y]    = intersect x y
-- jointDestinations' (x:y:xs) = jointDestinations' (intersect x y : xs)

-- TESTING
-- TODO: Utöka testning
-- Går det att automatisera, dvs unittesting. Kolla upp. 
p1 = Person "Vincent" 42 ["Sweden", "Norway", "Denmark"]
p2 = Person "Pontus" 32 ["Sweden", "Norway", "Germany"]
p3 = Person "Dante" 22 ["Sweden", "Spain", "Portugal"]
