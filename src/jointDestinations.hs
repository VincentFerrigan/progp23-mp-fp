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

jointDestinations :: [Person] -> [Destination]
jointDestinations ps = jointDestinations' $ map destinations ps

jointDestinations' :: [[Destination]] -> [Destination]
jointDestinations' []       = []
jointDestinations' [x]      = []
jointDestinations' [x,y]    = intersect x y
jointDestinations' (x:y:xs) = jointDestinations' (intersect x y : xs)
