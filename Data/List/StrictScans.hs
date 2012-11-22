-- | Element-forcing scans. See <http://hackage.haskell.org/trac/ghc/ticket/7203>

module Data.List.StrictScans (
    strictList
  , scanl'
  , scanl1'
) where


-- | Forces the elements of a list using `seq`. Does not force the whole list itself.
strictList :: [a] -> [a]
strictList [] = []
strictList (x:xs) = x `seq` (x : strictList xs)


-- | A strict version of `scanl`.
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f q ls = strictList $ scanl f q ls


-- | A strict version of `scanl1`.
scanl1'                  :: (a -> a -> a) -> [a] -> [a]
scanl1' f (x:xs)         =  scanl' f x xs
scanl1' _ []             =  []
