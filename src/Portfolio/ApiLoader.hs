-- src/Portfolio/ApiLoader.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Portfolio.ApiLoader (
    fetchReturnsMatrix
) where

import Control.Lens ((&), (.~), (^.))
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Maybe (catMaybes)
import Data.List (transpose, sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.Text as T
import Numeric.LinearAlgebra (Matrix, fromLists)
import Network.Wreq (getWith, defaults, param, responseBody)
import Data.Aeson.Lens (key, _JSON)
import Data.Text (Text)

import Portfolio.Types

fetchReturnsMatrix :: ApiKey
                   -> [StockSymbol]
                   -> Day
                   -> Day
                   -> IO (Maybe (Matrix Double))
fetchReturnsMatrix apiKey symbols start end = do
    -- Fetch the list of returns for each stock.
    maybeReturnsLists <- mapM (fetchSingleStockReturns apiKey start end) symbols

    -- If any stock failed, the whole operation fails.
    let maybeAlignedReturns = sequence maybeReturnsLists

    case maybeAlignedReturns of
        Nothing -> return Nothing
        -- Transpose the list of lists so that each ROW represents a day
        -- and each COLUMN represents a stock. This is the standard orientation.
        Just returnsLists -> return . Just . fromLists . transpose $ returnsLists

fetchSingleStockReturns :: ApiKey -> Day -> Day -> StockSymbol -> IO (Maybe [Double])
fetchSingleStockReturns apiKey start end symbol = do
    putStrLn $ "Fetching data for " ++ symbol ++ "..."
    let opts = defaults & param "function"   .~ ["TIME_SERIES_DAILY_ADJUSTED"]
                        & param "symbol"     .~ [T.pack symbol]
                        & param "outputsize" .~ ["full"]
                        -- This correctly uses the apiKey passed into the function
                        & param "apikey"     .~ [T.pack apiKey]

    r <- getWith opts "https://www.alphavantage.co/query"
    let mHistoricalData :: Maybe (Map.Map Text TimePoint) = r ^. responseBody . key "Time Series (Daily)" . _JSON

    case mHistoricalData of
        Nothing -> do
            putStrLn $ "Error: Failed to parse JSON for " ++ symbol ++ ". Check API key, call frequency, or symbol validity."
            print (r ^. responseBody)
            return Nothing
        Just dayMap -> do
            putStrLn $ "Successfully parsed data for " ++ symbol
            let sortedPrices = processAndFilter (Map.toList dayMap) start end
            if length sortedPrices < 2 then do
                putStrLn $ "Warning: Not enough data for " ++ symbol ++ " in the date range to calculate returns."
                return Nothing
            else
                return . Just . calculateReturns . map snd $ sortedPrices


processAndFilter :: [(Text, TimePoint)] -> Day -> Day -> [(Day, Double)]
processAndFilter rawList start end =
    let
        maybeParsed = map (\(dayStr, tp) -> fmap (, close tp) (parseDay (T.unpack dayStr))) rawList
        parsed = catMaybes maybeParsed
        filtered = filter (\(day, _) -> day >= start && day <= end) parsed
    in sortBy (comparing fst) filtered

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

calculateReturns :: [Double] -> [Double]
calculateReturns prices =
    zipWith (\p_prev p_curr -> (p_curr / p_prev) - 1.0) prices (tail prices)