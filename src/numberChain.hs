{- |
File name      : numberChain.hs
Module         : main
Description    : Task 3: Svansrekursiv sifferkedja
                 <https://canvas.kth.se/courses/38058/assignments/237975>
Course         : DD1360 VT23 Programmeringsparadigm (progp23)
Author/Student : Vincent Ferrigan
maintainer     : ferrigan@kth.se
-}

-- module Main
module NumberChain
    ( -- * Functions
      numberChain 
    ) where


numberChain :: Int -> Int -> [Int]
numberChain x y = reverse $ numberChain' (x:[]) y
  where
    numberChain' :: [Int] -> Int -> [Int]
    numberChain' ys@(x:xs) y 
      | x == y    = ys
      | x < y     = numberChain' (x+2:ys) y
      | otherwise = numberChain' (x-3:ys) y
                    

