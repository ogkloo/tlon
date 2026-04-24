module Tlon.Core.Rules (
    RoundRules (..),
)
where

import Tlon.Core.Event
import Tlon.Core.State
import Tlon.Core.Types

data RoundRules = RoundRules
    { roundRulesApplyOfferingPurchases :: [OfferingPurchase] -> GameState -> (GameState, [LotteryIssuance])
    , roundRulesApplyInstrumentSettlements :: GameState -> (GameState, [LotterySettlement])
    , roundRulesApplyRoundEffects :: GameState -> (GameState, [SurvivalResult], Maybe EntityId)
    , roundRulesGrantNextRound :: GameState -> (GameState, [Grant])
    , roundRulesActiveOfferingsForRound :: GameState -> Int -> [InstrumentOffering]
    }
