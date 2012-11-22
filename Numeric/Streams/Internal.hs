module Numeric.Streams.Internal (
    movingSum
  , movingSum1
  , movingSum2
  , movingSum3
  , movingAverage
  , movingAverageDrop
) where


import Data.List.StrictScans (scanl1')


-- Adapted from http://www.haskell.org/pipermail/haskell-cafe/2010-September/084171.html

movingSum1 :: Int -> [Double] -> [Double]
movingSum1 n _  | n < 0 = error "movingSum: Window size must be >= 0"
movingSum1 n xs         = zipWith (-) sumList shiftedSumList
  where
    sumList        = scanl1' (+) xs
    shiftedSumList = scanl1' (+) (replicate n 0.0 ++ xs)


movingSum2 :: (Num a) => Int -> [a] -> [a]
movingSum2 n _  | n < 0 = error "movingSum: Window size must be >= 0"
movingSum2 n xs         = zipWith (-) sumList shiftedSumList
  where
    zero           = fromInteger 0
    sumList        = scanl1' (+) xs
    shiftedSumList = scanl1' (+) (replicate n zero ++ xs)


-- | This is better than `movingSum2` because by taking the difference
-- first and *then* summing up, the values usually  do not become as big
-- (e.g. precision loss for floats or overflow for Int) as when
-- doing `scanl (+) xs` first and taking the difference later.
-- It is also around 2x faster.
movingSum3 :: (Num a) => Int -> [a] -> [a]
movingSum3 n _  | n < 0 = error "movingSum: Window size must be >= 0"
movingSum3 n xs         = scanl1' (+) $ zipWith (-) xs padded
  where
    zero   = fromInteger 0
    padded = replicate n zero ++ xs


-- | Calculates a moving sum with window size @n@.
--
-- Be aware of overflows and precision loss when using it with types
-- like 'Int' or 'Double'.
movingSum :: (Num a) => Int -> [a] -> [a]
movingSum = movingSum3


-- | Calculates a moving average with window size @n@,
-- dropping the first @n-1@ values (as the window needs to be filled).
--
-- See `movingAverage` for an alternative.
movingAverageDrop :: Int -> [Double] -> [Double]
movingAverageDrop n _  | n <= 0 = error "movingAverageDrop: Window size must be > 0"
movingAverageDrop n xs          = map (/ fromIntegral n) . drop (n - 1) $ movingSum n xs


-- | Calculates a moving average with window size @n@.
--
-- Uses the average of the values so far until the window is filled.
movingAverage :: Int -> [Double] -> [Double]
movingAverage n _  | n <= 0 = error "movingAverageDrop: Window size must be > 0"
movingAverage n xs          = [ x / i | (i, x) <- zip [1.0..] sumFirst ] ++ -- window being filled
                              [ x / fromIntegral n | x <- sumRest ]         -- window is filled
  where
    (sumFirst, sumRest) = splitAt n (movingSum n xs)
