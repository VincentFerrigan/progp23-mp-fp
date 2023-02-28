{- |
Module         : ListOfNumbers
File name      : list_of_numbers.hs 
Description    : Task 1: En lista med heltal
                 <https://canvas.kth.se/courses/38058/assignments/237975>

Course         : DD1360 VT23 Programmeringsparadigm (progp23)
Author/Student : Vincent Ferrigan
maintainer     : ferrigan@kth.se
-}

module ListOfNumbers
    ( -- * Functions
      squarePositive
    ) where

{- | Exempel

__Examples:__
@
>>> squarePositive [-2, -1, 0, 1, 2]
[1,4]

>>> squarePositive [1, 3, 2]
[1, 9, 4]

>>> squarePositive [0, 0, 0, 0]
[]

>>> squarePositive [-3, -5, -8, 2]
[4]
@
-}

-- | The 'squarePositive' function squares all positive integers in a list.
-- Both negative numbers and zero are ignored and excluded from the list.
squarePositive :: [Int] -> [Int]
squarePositive = map (^2) . filter (>0) 
-- squarePositive = map square . filter isPos 
-- squarePositive = map (\x -> x^2) . filter (\x -> x>0) 

-- square :: Int -> Int
-- square n = n * n

-- isPos :: Int -> Bool
-- isPos n | n > 0     = True
--         | otherwise = False
