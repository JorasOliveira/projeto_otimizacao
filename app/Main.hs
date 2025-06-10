-- app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (maximum, unfoldr) -- Added unfoldr
import Data.List.Split (chunksOf)
import Control.Monad (forM)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import GHC.Conc (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Numeric.LinearAlgebra ((¿), Matrix, toList, rows, fromList)
import System.Random (newStdGen, split, StdGen)

-- Local project modules
import Portfolio.Types
import Portfolio.DataLoader (loadReturnsMatrix)
import Portfolio.Utils (combinationsOf)
import Portfolio.Simulation (generateNWeights)
import Portfolio.Core (calculatePortfolioMetrics)

-- Configuration Constants
allStockSymbols :: [StockSymbol]
allStockSymbols = [ "AAPL", "AMGN", "AXP", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS", "DOW", "GS", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "NKE", "PG", "TRV", "UNH", "V", "VZ", "WBA", "WMT" ]

numStocksToChoose :: Int
numStocksToChoose = 25

numPortfoliosPerCombination :: Int
numPortfoliosPerCombination = 1000

trainingDataFile :: FilePath
trainingDataFile = "data/dow_returns_2024_h2.csv"

testingDataFile :: FilePath
testingDataFile = "data/dow_returns_2025_t1.csv"

-- | Main entry point
main :: IO ()
main = do
    putStrLn "--- Parallel Portfolio Optimizer ---"
    
    mTrainingMatrix <- loadReturnsMatrix trainingDataFile
    case mTrainingMatrix of
        Nothing -> putStrLn "Failed to load training data."
        Just trainingMatrix -> do
            putStrLn "Training data loaded. Starting truly parallel simulation..."
            
            let stockCombinations = combinationsOf numStocksToChoose allStockSymbols
            
            masterGen <- newStdGen
            let allGens = unfoldr (Just . split) masterGen
            
            let combinationsWithGens = zip stockCombinations allGens
            
            numCores <- getNumCapabilities
            let chunkSize = (length combinationsWithGens + numCores - 1) `div` numCores
            let combinationChunks = chunksOf chunkSize combinationsWithGens
            putStrLn $ "Processing " ++ show (length stockCombinations) ++ " combinations in " ++ show (length combinationChunks) ++ " chunks..."
            hFlush stdout

            bestPortfoliosFromChunks <- forConcurrently combinationChunks $ \chunk -> do
                putStrLn $ "-> Starting a new chunk of size " ++ show (length chunk)
                hFlush stdout
                
                let portfoliosInChunk = flip map chunk $ \(combination, gen) ->
                        let indices = findIndices combination allStockSymbols
                            combinationReturnsMat = trainingMatrix ¿ indices
                        in
                        if rows combinationReturnsMat < 2
                        then Portfolio { pStocks = [], pWeights = fromList [], pSharpe = -1/0, pReturn = 0, pVolatility = 0 }
                        else
                            let weightsList = generateNWeights gen (length combination) numPortfoliosPerCombination
                                portfolios = map (calculatePortfolioMetrics combinationReturnsMat combination) weightsList
                            in maximum portfolios

                return $! maximum portfoliosInChunk
            
            putStrLn "\nAll simulations complete. Finding the absolute best portfolio..."
            let bestPortfolioFromTraining = maximum bestPortfoliosFromChunks

            putStrLn "\n--- Optimal Portfolio Found (based on 2024 H2 data) ---"
            printPortfolio bestPortfolioFromTraining

            -- Phase 2: Testing
            putStrLn $ "\n--- Testing optimal portfolio on 2025 T1 data ---"
            hFlush stdout
            mTestingMatrix <- loadReturnsMatrix testingDataFile
            case mTestingMatrix of
                Nothing -> putStrLn "Failed to load testing data."
                Just testingMatrix -> do
                    let testIndices = findIndices (pStocks bestPortfolioFromTraining) allStockSymbols
                    let testReturnsMatrix = testingMatrix ¿ testIndices
                    if rows testReturnsMatrix < 2
                        then putStrLn "Not enough 2025 data to perform backtest."
                        else do
                            let testedPortfolio = calculatePortfolioMetrics testReturnsMatrix (pStocks bestPortfolioFromTraining) (pWeights bestPortfolioFromTraining)
                            putStrLn "\n--- Backtest Results (on 2025 T1 data) ---"
                            printPortfolio testedPortfolio

-- Helper functions that were missing
findIndices :: Eq a => [a] -> [a] -> [Int]
findIndices needles haystack = [i | (x, i) <- zip haystack [0..], x `elem` needles]

printPortfolio :: Portfolio -> IO ()
printPortfolio p = do
    printf "Sharpe Ratio:  %.4f\n" (pSharpe p)
    printf "Annual Return: %.2f%%\n" (pReturn p * 100)
    printf "Annual Vol:    %.2f%%\n" (pVolatility p * 100)
    putStrLn "Stocks and Weights:"
    let weightedStocks = zip (toList $ pWeights p) (pStocks p)
    mapM_ (\(w, s) -> printf "  %-5s: %.2f%%\n" s (w * 100)) weightedStocks