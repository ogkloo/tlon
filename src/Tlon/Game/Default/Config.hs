module Tlon.Game.Default.Config
  ( DefaultConfig (..),
    defaultConfig,
  )
where

import Tlon.Core.Types

data DefaultConfig = DefaultConfig
  { configPlayerCount :: Int,
    configStartingAccessTokens :: Quantity,
    configInitialBundleMax :: Quantity,
    configGovernmentReserve :: Quantity,
    configRoundGrantQuantity :: Quantity,
    configInitialSeed :: Int,
    configMatchingPolicy :: MatchingPolicy
  }
  deriving (Eq, Show)

defaultConfig :: DefaultConfig
defaultConfig =
  DefaultConfig
    { configPlayerCount = 4,
      configStartingAccessTokens = 5,
      configInitialBundleMax = 2,
      configGovernmentReserve = 1000,
      configRoundGrantQuantity = 1,
      configInitialSeed = 17,
      configMatchingPolicy = PriceTimeFIFO
    }
