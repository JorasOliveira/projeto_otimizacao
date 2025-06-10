-- src/Portfolio/DataLoader.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Portfolio.DataLoader (
    loadReturnsMatrix
) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Matrix, fromLists)
import Data.Maybe (catMaybes) -- Import catMaybes

-- | Loads a returns matrix from a CSV file where columns are stock symbols.
loadReturnsMatrix :: FilePath -> IO (Maybe (Matrix Double))
loadReturnsMatrix filepath = do
    csvData <- BL.readFile filepath
    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "Error parsing CSV file " ++ filepath ++ ": " ++ err
            return Nothing
        Right (rows :: V.Vector (V.Vector String)) -> do
            let stringRows = V.toList $ V.map (V.toList . V.tail) rows
            
            let maybeGoodRows = map (sequence . map (readMaybe :: String -> Maybe Double)) stringRows
            
            let goodRows = catMaybes maybeGoodRows

            if null goodRows
                then do
                    putStrLn $ "Error: No valid data rows could be parsed from " ++ filepath
                    return Nothing
                else
                    return . Just . fromLists $ goodRows

-- Helper to safely read a string into a number.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing