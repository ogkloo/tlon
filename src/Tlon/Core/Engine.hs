module Tlon.Core.Engine (
    stepRound,
)
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Tlon.Core.Event
import Tlon.Core.OrderBook
import Tlon.Core.Rules
import Tlon.Core.State
import Tlon.Core.Types

stepRound :: RoundRules -> RoundInputs -> GameState -> (GameState, [GameEvent])
stepRound rules inputs state =
    let submissions = roundOrders inputs
        offeringPurchases = roundOfferingPurchases inputs
        (validatedOrders, invalidOrders) = validateOrders state submissions
        fills = matchRound (gameMatchingPolicy state) validatedOrders
        expiredOrders = computeExpiredOrders validatedOrders fills
        settledState = state{gameHoldings = settleFills state (gameHoldings state) fills}
        (issuedState, lotteryIssuances) = roundRulesApplyOfferingPurchases rules offeringPurchases settledState
        (lotteryState, lotterySettlements) = roundRulesApplyInstrumentSettlements rules issuedState
        (survivedState, survivalResults, refundRecipient) = roundRulesApplyRoundEffects rules lotteryState
        winner = determineWinner survivedState
        nextRoundNumber = gameRoundNumber state + 1
        advancedState =
            survivedState
                { gameRoundNumber = nextRoundNumber
                , gameWinner = winner
                }
        (finalState, grants, nextActiveOfferings) =
            case winner of
                Nothing ->
                    let (grantedState, granted) = roundRulesGrantNextRound rules advancedState
                        offerings = roundRulesActiveOfferingsForRound rules grantedState nextRoundNumber
                     in (grantedState{gameActiveOfferings = offerings}, granted, offerings)
                Just _ -> (advancedState{gameActiveOfferings = []}, [], [])
        report =
            RoundReport
                { reportRoundNumber = gameRoundNumber state
                , reportActiveOfferings = gameActiveOfferings state
                , reportSubmittedOrders = submissions
                , reportOfferingPurchases = offeringPurchases
                , reportInvalidOrders = invalidOrders
                , reportFills = fills
                , reportExpiredOrders = expiredOrders
                , reportLotteryIssuances = lotteryIssuances
                , reportLotterySettlements = lotterySettlements
                , reportSurvivalResults = survivalResults
                , reportRefundRecipient = refundRecipient
                , reportNextRoundGrants = grants
                , reportNextActiveOfferings = nextActiveOfferings
                }
        stateWithReport = finalState{gamePreviousReport = Just report}
     in (stateWithReport, [RoundResolved report])

validateOrders :: GameState -> [Order] -> ([ValidatedOrder], [InvalidOrder])
validateOrders state orders =
    let folder (validated, invalids, reserved) (submissionIndex, order) =
            case validateOrder state reserved order of
                Left reason -> (validated, invalids ++ [InvalidOrder order reason], reserved)
                Right reserved' ->
                    ( validated ++ [ValidatedOrder order submissionIndex]
                    , invalids
                    , reserved'
                    )
        (validatedOrders, invalidOrders, _) = foldl folder ([], [], Map.empty) (zip [0 ..] orders)
     in (validatedOrders, invalidOrders)

validateOrder ::
    GameState ->
    Holdings ->
    Order ->
    Either InvalidReason Holdings
validateOrder state reserved order
    | not (isActiveOrderingEntity state order) = Left InactiveEntity
    | orderQuantity order <= 0 = Left NonPositiveQuantity
    | orderLimitPrice order <= 0 = Left NonPositivePrice
    | not (isPermittedPair state order) = Left UnsupportedPair
    | not (satisfiesMarketRules state order) = Left ViolatesMarketRule
    | not (hasSufficientInventory state reserved order) = Left InsufficientInventory
    | otherwise = Right (reserveOrder state reserved order)

isActiveOrderingEntity :: GameState -> Order -> Bool
isActiveOrderingEntity state order =
    case Map.lookup (orderEntityId order) (gameEntities state) of
        Just entity -> entityAlive entity
        Nothing -> False

isPermittedPair :: GameState -> Order -> Bool
isPermittedPair state order =
    case Map.lookup (orderMarketId order) (gameMarkets state) of
        Nothing -> False
        Just market -> (orderBaseAsset order, orderQuoteAsset order) `elem` marketPairs market

satisfiesMarketRules :: GameState -> Order -> Bool
satisfiesMarketRules state order =
    case Map.lookup (orderMarketId order) (gameMarkets state) of
        Nothing -> False
        Just market -> all (marketRuleAllowsOrder state market order) (marketRules market)

marketRuleAllowsOrder :: GameState -> Market -> Order -> MarketRule -> Bool
marketRuleAllowsOrder state market order rule =
    case rule of
        QuoteAssetMustBeOwnerIssuedCurrency ->
            case baseAssetIdForSeries state quoteSeries of
                Nothing -> False
                Just quoteAsset ->
                    isCurrencyAsset quoteAsset
                        && Map.lookup quoteAsset (gameAssetIssuers state) == Just (marketOwner market)
  where
    quoteSeries = orderQuoteAsset order

hasSufficientInventory :: GameState -> Holdings -> Order -> Bool
hasSufficientInventory state reserved order =
    let entityId' = orderEntityId order
        seriesToReserve =
            case orderSide order of
                Buy -> orderQuoteAsset order
                Sell -> orderBaseAsset order
        amountToReserve =
            case orderSide order of
                Buy -> orderQuantity order * orderLimitPrice order
                Sell -> orderQuantity order
        available = balanceOf (gameHoldings state) entityId' seriesToReserve
        alreadyReserved = balanceOf reserved entityId' seriesToReserve
     in available - alreadyReserved >= amountToReserve

reserveOrder :: GameState -> Holdings -> Order -> Holdings
reserveOrder _ reserved order =
    case orderSide order of
        Buy ->
            adjustBalance
                (orderEntityId order)
                (orderQuoteAsset order)
                (orderQuantity order * orderLimitPrice order)
                reserved
        Sell ->
            adjustBalance
                (orderEntityId order)
                (orderBaseAsset order)
                (orderQuantity order)
                reserved

settleFills :: GameState -> Holdings -> [Fill] -> Holdings
settleFills _ =
    foldl settleOneFill

settleOneFill :: Holdings -> Fill -> Holdings
settleOneFill holdings fill =
    let quoteAmount = fillQuantity fill * fillPrice fill
     in adjustBalance (fillBuyer fill) (fillBaseAsset fill) (fillQuantity fill)
            . adjustBalance (fillSeller fill) (fillBaseAsset fill) (negate (fillQuantity fill))
            . adjustBalance (fillSeller fill) (fillQuoteAsset fill) quoteAmount
            . adjustBalance (fillBuyer fill) (fillQuoteAsset fill) (negate quoteAmount)
            $ holdings

computeExpiredOrders :: [ValidatedOrder] -> [Fill] -> [ExpiredOrder]
computeExpiredOrders validatedOrders fills =
    let filledByOrderId =
            foldl
                ( \filled fill ->
                    Map.insertWith (+) (fillBuyOrderId fill) (fillQuantity fill)
                        . Map.insertWith (+) (fillSellOrderId fill) (fillQuantity fill)
                        $ filled
                )
                Map.empty
                fills
        remainderFor validated =
            let order = validatedOrder validated
                filled = Map.findWithDefault 0 (orderId order) filledByOrderId
                remaining = orderQuantity order - filled
             in if remaining > 0
                    then Just (ExpiredOrder (orderId order) (orderEntityId order) remaining)
                    else Nothing
     in foldr
            ( \validated acc ->
                case remainderFor validated of
                    Nothing -> acc
                    Just expired -> expired : acc
            )
            []
            validatedOrders

determineWinner :: GameState -> Maybe EntityId
determineWinner state =
    case livingMortals state of
        [] -> Just (findGovernmentId state)
        [soleWinner] -> Just soleWinner
        _ -> Nothing
