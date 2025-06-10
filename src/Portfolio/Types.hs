-- src/Portfolio/Types.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Portfolio.Types where

import GHC.Generics (Generic)
-- Add ToJSON, object, and (.=) to the import
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), object, (.=))
import Data.Time (Day)
import Data.Map (Map)
import Numeric.LinearAlgebra (Vector)

type StockSymbol = String
type ApiKey = String
type Returns = Vector Double
type Weights = Vector Double
type Sharpe = Double

data TimePoint = TimePoint {
    close :: Double
} deriving (Show, Generic) -- Removed ToJSON from here

instance FromJSON TimePoint where
    parseJSON = withObject "TimePoint" $ \v -> TimePoint
        <$> fmap read (v .: "4. close")

-- Add the manual ToJSON instance
instance ToJSON TimePoint where
    -- We must convert the Double back to a String to match the API's format
    toJSON (TimePoint c) = object ["4. close" .= show c]

data HistoricalData = HistoricalData {
    timeSeries :: Map Day TimePoint
} deriving (Show, Generic)

instance FromJSON HistoricalData where
    parseJSON = withObject "HistoricalData" $ \v -> HistoricalData
        <$> v .: "Time Series (Daily)"

data Portfolio = Portfolio {
    pStocks    :: [StockSymbol],
    pWeights   :: Weights,
    pSharpe    :: Sharpe,
    pReturn    :: Double,
    pVolatility:: Double
} deriving (Show)

instance Eq Portfolio where
    (==) p1 p2 = pSharpe p1 == pSharpe p2

instance Ord Portfolio where
    compare p1 p2 = compare (pSharpe p1) (pSharpe p2)