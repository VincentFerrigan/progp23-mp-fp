{- |
File name      : jointDestinations.hs
Module         : main
Description    : Task 2: Resmål
                 <https://canvas.kth.se/courses/38058/assignments/237975>
Course         : DD1360 VT23 Programmeringsparadigm (progp23)
Author/Student : Vincent Ferrigan
maintainer     : ferrigan@kth.se
-}

module Main
    ( -- * Data type
      Person
      -- * Functions
    -- , jointDestinations
    ) where

type Destination = [Char]

data Person = Person
    { name         :: [Char] 
    , age          :: Int
    , destinations :: [Destination]
    }

-- jointDestinations :: [Person] -> [String]
