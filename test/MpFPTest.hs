module Main (main) where
import Test.HUnit
import MpFP
import qualified System.Exit as Exit

tests :: Test
tests = TestList [ TestLabel "test1a" test1a
                 , TestLabel "test1b" test1b
                 , TestLabel "test1c" test1c
                 , TestLabel "test1d" test1d

                 -- , TestLabel "test2a" test2a

                 , TestLabel "test3a" test3a
                 , TestLabel "test3b" test3b
                 , TestLabel "test3c" test3c
                 , TestLabel "test3d" test3d
                 , TestLabel "test3e" test3e
                 , TestLabel "test3f" test3f

                 , TestLabel "test4a" test4a
                 , TestLabel "test4b" test4b
                 , TestLabel "test4c" test4c
                 , TestLabel "test4d" test4d
                 , TestLabel "test4e" test4e
                 , TestLabel "test4f" test4f
                 ]

-- Test for TASK 1 En lista med heltal
test1a :: Test
test1a = TestCase (assertEqual "Should return [1,4]" [1,4] (squarePositive [-2, -1, 0, 1, 2]))

test1b :: Test
test1b = TestCase (assertEqual "Should return [1, 9, 4]" [1, 9, 4] (squarePositive [1, 3, 2]))

test1c :: Test
test1c = TestCase (assertEqual "Should return []" [] (squarePositive [0, 0, 0, 0]))

test1d :: Test
test1d = TestCase (assertEqual "Should return [4]" [4] (squarePositive [-3, -5, -8, 2]))

-- Test for TASK 2 Resmål

-- test2a :: Test
-- -- test2a = TestCase (assertEqual "Should return [\"Denmark\",\"Germany\",\"Norway",\"Portugal\",\"Spain\",\"Sweden\"]" ["Denmark","Germany","Norway","Portugal","Spain","Sweden"] 
-- test2a = TestCase (assertEqual "Should return []" ["Denmark","Germany","Norway","Portugal","Spain","Sweden"] 
--   (jointDestinations [ Person "Vincent"  42 ["Sweden", "Norway", "Denmark"]
--                      , Person "Pontus" 32 ["Sweden", "Norway", "Germany"]
--                      , Person "Dante" 22 ["Sweden", "Spain", "Portugal"]
--                      ]))

-- Test for TASK 3 Svansrekursiv sifferkedja
test3a :: Test
test3a = TestCase (assertEqual "Should return [3, 5, 7, 9, 11, 13, 15, 17, 14, 16]" [3, 5, 7, 9, 11, 13, 15, 17, 14, 16] (numberChain 3 16))

test3b :: Test
test3b = TestCase (assertEqual "Should return [1, 3, 0, 2]" [1, 3, 0, 2] (numberChain 1 2))

test3c :: Test
test3c = TestCase (assertEqual "Should return [1]" [1] (numberChain 1 1))

test3d :: Test
test3d = TestCase (assertEqual 
  "Should return [17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 40, 42]" 
  [17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 40, 42] (numberChain 17 42))

test3e :: Test
test3e = TestCase (assertEqual "Should return [-1, -4, -7, -10]" [-1, -4, -7, -10] (numberChain (-1) (-10)))

test3f :: Test
test3f = TestCase (assertEqual "Should return [-1, -4, -7, -10, -8, -11, -9]" [-1, -4, -7, -10, -8, -11, -9] (numberChain (-1) (-9)))

-- Test for TASK 4 Kortaste vägen
test4a :: Test
test4a = TestCase (assertEqual "Should return 29.953845823305446" 29.953845823305446 (totalDistance [(1.3, 2.4), (5.3, -1.3), (-4.2, -3.4), (5.2, 8.0)]))

test4b :: Test
test4b = TestCase (assertEqual "Should return 5.448853090330111" 5.448853090330111 (totalDistance [(1.3, 2.4), (5.3, -1.3)]))

test4c :: Test
test4c = TestCase (assertEqual "Should return 9.729337079164232" 9.729337079164232 (totalDistance [(5.3, -1.3), (-4.2, -3.4)]))

test4d :: Test
test4d = TestCase (assertEqual "Should return 14.775655653811103" 14.775655653811103 (totalDistance [(-4.2, -3.4), (5.2, 8.0)]))

test4e :: Test
test4e = TestCase (assertEqual "Should return 0.0" 0.0 (totalDistance []))

test4f :: Test
test4f = TestCase (assertEqual "Should return 0.0" 0.0 (totalDistance [(3.24, 14.23)]))

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
