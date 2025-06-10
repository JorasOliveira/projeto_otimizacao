-- src/Portfolio/Utils.hs
module Portfolio.Utils (
    combinationsOf
) where

-- | combinationsOf k xs returns the list of all k-element sublists of xs.
--   Example: combinationsOf 2 "ABC" -> ["AB", "AC", "BC"]
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _      = [[]]
combinationsOf _ []     = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs