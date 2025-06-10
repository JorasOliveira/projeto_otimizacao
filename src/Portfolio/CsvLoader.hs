-- src/Portfolio/CsvLoader.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Portfolio.CsvLoader (
    loadReturnsMatrix
) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Time (Day)
import Data.List (transpose, sortBy, groupBy, find)
import Data.Ord (comparing)
import Data.Function (on)
import Numeric.LinearAlgebra (Matrix, fromLists)

import Portfolio.Types

-- A record to match the structure of our CSV file
data StockRecord = StockRecord {
    recDate   :: Day,
    recClose  :: Double,
    recSymbol :: StockSymbol
} deriving (Show)

-- Tell Cassava how to parse a row from the CSV into our record.
-- It needs to match the column order: Date, Open, High, Low, Close, Volume, Name
instance FromNamedRecord StockRecord where
    parseNamedRecord r = StockRecord
        <$> r .: "Date"
        <*> r .: "Close"
        <*> r .: "Name"

-- | The main function for this module.
loadReturnsMatrix :: FilePath
                  -> [StockSymbol]
                  -> Day
                  -> Day
                  -> IO (Maybe (Matrix Double))
loadReturnsMatrix filepath symbols start end = do
    csvData <- BL.readFile filepath
    case decodeByName csvData of
        Left err -> do
            putStrLn $ "Error parsing CSV: " ++ err
            return Nothing
        Right (_, v) -> do
            let allRecords = V.toList v
            -- 1. Filter records to only include the symbols and date range we want.
            let relevantRecords = filter (isRelevant symbols start end) allRecords
            -- 2. Group records by stock symbol.
            let groupedBySymbol = groupBy ((==) `on` recSymbol) . sortBy (comparing recSymbol) $ relevantRecords
            -- 3. For each stock, sort its prices by date and calculate returns.
            let returnsPerStock = map processGroup groupedBySymbol
            -- 4. Check if we found data for all the symbols we asked for.
            if length returnsPerStock /= length symbols then do
                 putStrLn "Warning: Missing data for some stocks in the CSV."
                 return Nothing
            else
                 -- 5. Transpose and convert to a Matrix.
                 return . Just . fromLists . transpose $ returnsPerStock

isRelevant :: [StockSymbol] -> Day -> Day -> StockRecord -> Bool
isRelevant symbols start end rec =
    recSymbol rec `elem` symbols && recDate rec >= start && recDate rec <= end

processGroup :: [StockRecord] -> [Double]
processGroup group =
    let sorted = sortBy (comparing recDate) group
        prices = map recClose sorted
    in calculateReturns prices

calculateReturns :: [Double] -> [Double]
calculateReturns prices =
    zipWith (\p_prev p_curr -> (p_curr / p_prev) - 1.0) prices (tail prices)