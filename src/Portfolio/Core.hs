-- src/Portfolio/Core.hs
module Portfolio.Core (
    calculatePortfolioMetrics
) where

import Numeric.LinearAlgebra.Data (Matrix)
import Numeric.LinearAlgebra ((#>))
import Statistics.Sample (mean, variance)
import Portfolio.Types

tradingDays :: Double
tradingDays = 252.0

calculatePortfolioReturn :: Matrix Double -> Weights -> Double
calculatePortfolioReturn returnsMat weights =
    let portfolioDailyReturns = returnsMat #> weights
    in mean portfolioDailyReturns * tradingDays

calculatePortfolioVolatility :: Matrix Double -> Weights -> Double
calculatePortfolioVolatility returnsMat weights =
    let portfolioDailyReturns = returnsMat #> weights
        dailyVariance = variance portfolioDailyReturns
    in sqrt (max 0 dailyVariance) * sqrt tradingDays

-- FIX: Rename arguments to 'ret' and 'vol' to avoid shadowing.
calculateSharpeRatio :: Double -> Double -> Sharpe
calculateSharpeRatio ret vol
    | vol < 1e-9 = 0.0
    | otherwise  = ret / vol

calculatePortfolioMetrics :: Matrix Double -> [StockSymbol] -> Weights -> Portfolio
calculatePortfolioMetrics returnsMatrix stocks weights =
    let
        pRet = calculatePortfolioReturn returnsMatrix weights
        pVol = calculatePortfolioVolatility returnsMatrix weights
        -- FIX: Use a different name for the calculated Sharpe value.
        sharpeValue = calculateSharpeRatio pRet pVol
    in Portfolio {
        pStocks = stocks,
        pWeights = weights,
        pSharpe = sharpeValue,
        pReturn = pRet,
        pVolatility = pVol
    }