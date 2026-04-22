module Tlon.Core.Engine (
    stepRound,
)
where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Tlon.Core.Event
import Tlon.Core.OrderBook
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Game.Default.Config
import Tlon.Game.Default.Rules

stepRound :: DefaultConfig -> RoundInputs -> GameState -> (GameState, [GameEvent])
stepRound config inputs state =
    let submissions = roundOrders inputs
        (validatedOrders, invalidOrders) = validateOrders state submissions
        fills = matchRound (gameMatchingPolicy state) validatedOrders
        expiredOrders = computeExpiredOrders validatedOrders fills
        settledState = state{gameHoldings = settleFills (gameHoldings state) fills}
        (redeemedState, redemptions) = applyRedemptions settledState
        (survivedState, survivalResults, refundRecipient) = applySurvival redeemedState
        winner = determineWinner survivedState
        nextRoundNumber = gameRoundNumber state + 1
        advancedState =
            survivedState
                { gameRoundNumber = nextRoundNumber
                , gameWinner = winner
                }
        (finalState, grants, nextRedemptionTable) =
            case winner of
                Nothing ->
                    let (grantedState, granted) = grantNextRoundTokens (configRoundGrantQuantity config) advancedState
                     in (grantedState{gameRedemptionTable = redemptionForRound nextRoundNumber}, granted, redemptionForRound nextRoundNumber)
                Just _ -> (advancedState, [], Map.empty)
        report =
            RoundReport
                { reportRoundNumber = gameRoundNumber state
                , reportRedemptionTable = gameRedemptionTable state
                , reportSubmittedOrders = submissions
                , reportInvalidOrders = invalidOrders
                , reportFills = fills
                , reportExpiredOrders = expiredOrders
                , reportRedemptions = redemptions
                , reportSurvivalResults = survivalResults
                , reportRefundRecipient = refundRecipient
                , reportNextRoundGrants = grants
                , reportNextRedemptionTable = nextRedemptionTable
                }
        stateWithReport = finalState{gamePreviousReport = Just report}
     in (stateWithReport, [RoundResolved report])

validateOrders :: GameState -> [Order] -> ([ValidatedOrder], [InvalidOrder])
validateOrders state orders =
    let folder (validated, invalids, reserved, seenSides) (submissionIndex, order) =
            case validateOrder state reserved seenSides order of
                Left reason -> (validated, invalids ++ [InvalidOrder order reason], reserved, seenSides)
                Right (reserved', seenSides') ->
                    ( validated ++ [ValidatedOrder order submissionIndex]
                    , invalids
                    , reserved'
                    , seenSides'
                    )
        (validatedOrders, invalidOrders, _, _) = foldl folder ([], [], Map.empty, Map.empty) (zip [0 ..] orders)
     in (validatedOrders, invalidOrders)

validateOrder ::
    GameState ->
    Holdings ->
    Map (EntityId, MarketId, AssetId, AssetId) Side ->
    Order ->
    Either InvalidReason (Holdings, Map (EntityId, MarketId, AssetId, AssetId) Side)
validateOrder state reserved seenSides order
    | not (isActiveOrderingEntity state order) = Left InactiveEntity
    | orderQuantity order <= 0 = Left NonPositiveQuantity
    | orderLimitPrice order <= 0 = Left NonPositivePrice
    | not (isPermittedPair state order) = Left UnsupportedPair
    | not (satisfiesMarketRules state order) = Left ViolatesMarketRule
    | hasOpposingSide seenSides order = Left OpposingSidesSamePair
    | not (hasSufficientInventory state reserved order) = Left InsufficientInventory
    | otherwise =
        let reserved' = reserveOrder reserved order
            pairKey = (orderEntityId order, orderMarketId order, orderBaseAsset order, orderQuoteAsset order)
            seenSides' = Map.insert pairKey (orderSide order) seenSides
         in Right (reserved', seenSides')

isActiveOrderingEntity :: GameState -> Order -> Bool
isActiveOrderingEntity state order =
    case Map.lookup (orderEntityId order) (gameEntities state) of
        Just entity -> entityKind entity == PlayerEntity && entityAlive entity
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
            isCurrencyAsset quoteAsset
                && Map.lookup quoteAsset (gameAssetIssuers state) == Just (marketOwner market)
  where
    quoteAsset = orderQuoteAsset order

hasOpposingSide :: Map (EntityId, MarketId, AssetId, AssetId) Side -> Order -> Bool
hasOpposingSide seenSides order =
    case Map.lookup pairKey seenSides of
        Nothing -> False
        Just seenSide -> seenSide /= orderSide order
  where
    pairKey = (orderEntityId order, orderMarketId order, orderBaseAsset order, orderQuoteAsset order)

hasSufficientInventory :: GameState -> Holdings -> Order -> Bool
hasSufficientInventory state reserved order =
    let entityId' = orderEntityId order
        assetToReserve =
            case orderSide order of
                Buy -> orderQuoteAsset order
                Sell -> orderBaseAsset order
        amountToReserve =
            case orderSide order of
                Buy -> orderQuantity order * orderLimitPrice order
                Sell -> orderQuantity order
        available = balanceOf (gameHoldings state) entityId' assetToReserve
        alreadyReserved = balanceOf reserved entityId' assetToReserve
     in available - alreadyReserved >= amountToReserve

reserveOrder :: Holdings -> Order -> Holdings
reserveOrder reserved order =
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

settleFills :: Holdings -> [Fill] -> Holdings
settleFills =
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
