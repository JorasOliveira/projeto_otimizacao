-- src/Portfolio/Simulation.hs
module Portfolio.Simulation (
    generateNWeights
) where

import System.Random (StdGen, randoms)
-- ** THE FIX IS HERE: Added toList **
import Numeric.LinearAlgebra (fromList, toList)
import Data.List (find)
import Data.Maybe (isJust)

import Portfolio.Types

-- | A PURE function to generate N valid weight vectors from a given random source.
generateNWeights :: StdGen -- ^ The random number generator for this thread
                 -> Int    -- ^ Number of stocks
                 -> Int    -- ^ N, the number of weights to generate
                 -> [Weights]
generateNWeights gen numStocks n =
    take n (allValidWeights gen numStocks)

-- | Creates an infinite lazy list of valid portfolio weights.
allValidWeights :: StdGen -> Int -> [Weights]
allValidWeights gen numStocks =
    let allRandomDoubles = randoms gen :: [Double]
        randChunks = chunksOf numStocks allRandomDoubles
        normalizedChunks = map normalizeChunk randChunks
    in filter isValidWeightVector normalizedChunks

-- | Normalizes a chunk of random numbers to sum to 1.
normalizeChunk :: [Double] -> Weights
normalizeChunk rands =
    let total = sum rands
    in if total == 0.0
        then fromList (replicate (length rands) 0.0)
        else fromList $ map (/ total) rands

-- | Checks if a weight vector is valid (no element > 0.2).
isValidWeightVector :: Weights -> Bool
isValidWeightVector = not . isJust . find (> 0.2) . toList

-- | Helper to break a list into chunks.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)