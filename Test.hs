module Main where


import Criterion.Main
import Numeric.Streams.Internal
import Test.Hspec
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck
import Test.HUnit


benchmark :: IO ()
benchmark = defaultMain $ [ bench (mkTitle name desc) $ nf (fun _WINDOW_SIZE) [1.0.._LEN]
                          | (name, fun, desc) <- functions ]
  where
    _WINDOW_SIZE = 1000
    _LEN         = 200000
    functions = [
        ("movingSum1", movingSum1, "zipWith (-) . scanl (+) - specialized to [Double]")
      , ("movingSum2", movingSum2, "zipWith (-) . scanl (+) - generic on [Num]")
      , ("movingSum3", movingSum3, "scanl (+) . zipWith (-) - generic on [Num]")
      ]
    mkTitle funName desc = funName ++ " " ++ show _WINDOW_SIZE ++ " [1.." ++ show _LEN ++ "]"
                           ++ "(" ++ desc ++ ")"


main :: IO ()
main = do

  -- Enable for benchmarking
  -- benchmark

  hspec $ do

    describe "movingSum" $ do

      it "movingSum 5 [1..10]" $
        movingSum 5 [1..10] @?= [1.0, 3.0, 6.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0 :: Double]

      it "gives the same as sum for windows >= list size" $ property $
        \xs -> let sums = movingSum (length xs) xs in case (xs :: [Double]) of
                 [] -> sums == []
                 _  -> last sums == sum xs

      it "does not space leak (this should run quickly and in constant space)" $
        last (movingSum 4 [1..50000000 :: Integer]) @?= 199999994

    describe "movingAverage" $ do

      it "movingAverage 2 [1..1000000]" $
        movingAverage 2 [1..1000000] == (1.0):[1.5..999999.5]

      it "works even before the window is filled" $
        movingAverage 4 [1..8] @?= [1.0, 1.5, 2.0, 2.5, 3.5, 4.5, 5.5, 6.5]


    describe "movingAverageDrop" $ do

      it "drops the first n values" $
        movingAverageDrop 4 [1..8] @?= [2.5, 3.5, 4.5, 5.5, 6.5]

