module Tlon.Game.Default.Scenario (
    guaranteedLotteryOfferings,
    guaranteedLotteryState,
)
where

import qualified Data.Map.Strict as Map
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Game.Default.Config
import Tlon.Game.Default.Setup

-- A minimal teaching scenario: one human player, the government, no market,
-- and a single guaranteed lottery offering that doubles TLN001.
guaranteedLotteryState :: DefaultConfig -> GameState
guaranteedLotteryState config =
    let state = initialState config
        governmentId = findGovernmentId state
     in state
            { gameMarkets = Map.empty
            , gameActiveOfferings = guaranteedLotteryOfferings governmentId
            }

guaranteedLotteryOfferings :: EntityId -> [InstrumentOffering]
guaranteedLotteryOfferings governmentId =
    [ InstrumentOffering
        { instrumentOfferingSeriesId = ticketSeriesId 1 1 (assetSeriesId TLN001) 1 1 1 2
        , instrumentOfferingIssuer = governmentId
        , instrumentOfferingTerms =
            LotteryOffering
                LotteryOfferingTerms
                    { lotteryOfferingPayoutSeriesId = assetSeriesId TLN001
                    , lotteryOfferingTicketPrice = 1
                    , lotteryOfferingOddsNumerator = 1
                    , lotteryOfferingOddsDenominator = 1
                    , lotteryOfferingPayoutQuantity = 2
                    , lotteryOfferingDurationRounds = 1
                    }
        }
    ]
