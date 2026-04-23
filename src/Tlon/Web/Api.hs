{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.Api (
    gameJson,
    reportJson,
)
where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import Tlon.Core.Event
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Web.State

gameJson :: RunningGame -> Value
gameJson runningGame =
    let state = runningState runningGame
     in object
            [ "gameId" .= showGameId (runningGameId runningGame)
            , "started" .= runningStarted runningGame
            , "seatCount" .= runningSeatCount runningGame
            , "humanSeatCount" .= humanSeatCount runningGame
            , "npcCount" .= runningNpcCount runningGame
            , "participants" .= map renderHumanPlayer (runningParticipants runningGame)
            , "roundNumber" .= gameRoundNumber state
            , "winner" .= fmap showEntityId (gameWinner state)
            , "lotteryMenu" .= map renderLotteryOffer (gameLotteryMenu state)
            , "markets" .= map (renderMarket state) (Map.elems (gameMarkets state))
            , "entities" .= map (entitySummary state) (Map.elems (gameEntities state))
            ]

reportJson :: RoundReport -> Value
reportJson report =
    object
        [ "roundNumber" .= reportRoundNumber report
        , "lotteryMenu" .= map renderLotteryOffer (reportLotteryMenu report)
        , "submittedOrders" .= map renderOrder (reportSubmittedOrders report)
        , "lotteryPurchases" .= map renderLotteryPurchase (reportLotteryPurchases report)
        , "invalidOrders" .= map renderInvalidOrder (reportInvalidOrders report)
        , "fills" .= map renderFill (reportFills report)
        , "expiredOrders" .= map renderExpiredOrder (reportExpiredOrders report)
        , "lotteryIssuances" .= map renderLotteryIssuance (reportLotteryIssuances report)
        , "lotterySettlements" .= map renderLotterySettlement (reportLotterySettlements report)
        , "survivalResults" .= map renderSurvivalResult (reportSurvivalResults report)
        , "refundRecipient" .= fmap showEntityId (reportRefundRecipient report)
        , "nextRoundGrants" .= map renderGrant (reportNextRoundGrants report)
        , "nextLotteryMenu" .= map renderLotteryOffer (reportNextLotteryMenu report)
        ]

entitySummary :: GameState -> Entity -> Value
entitySummary state entity =
    object
        [ "entityId" .= showEntityId (entityId entity)
        , "name" .= entityName entity
        , "kind" .= show (entityKind entity)
        , "alive" .= entityAlive entity
        , "holdings" .= map renderSeriesQuantity (Map.toList (Map.findWithDefault Map.empty (entityId entity) (gameHoldings state)))
        ]

renderHumanPlayer :: HumanPlayer -> Value
renderHumanPlayer participant =
    object
        [ "playerId" .= showPlayerId (humanPlayerId participant)
        , "name" .= humanPlayerName participant
        , "entityId" .= fmap showEntityId (humanPlayerEntityId participant)
        ]

renderMarket :: GameState -> Market -> Value
renderMarket state market =
    object
        [ "marketId" .= showMarketId (marketId market)
        , "name" .= marketName market
        , "owner" .= showEntityId (marketOwner market)
        , "pairs" .= map renderMarketPair (marketPairs market)
        , "rules" .= map show (marketRules market)
        , "ownerIssuedCurrencies" .= map show (ownerIssuedCurrencies state market)
        ]

renderMarketPair :: (SeriesId, SeriesId) -> Value
renderMarketPair (baseAsset, quoteAsset) =
    object
        [ "baseAsset" .= baseAsset
        , "quoteAsset" .= quoteAsset
        ]

ownerIssuedCurrencies :: GameState -> Market -> [AssetId]
ownerIssuedCurrencies state market =
    [ assetId
    | (assetId, issuerId) <- Map.toList (gameAssetIssuers state)
    , issuerId == marketOwner market
    , isCurrencyAsset assetId
    ]

renderSeriesQuantity :: (SeriesId, Quantity) -> Value
renderSeriesQuantity (seriesId, quantity) =
    object
        [ "assetId" .= seriesId
        , "quantity" .= quantity
        ]

renderOrder :: Order -> Value
renderOrder order =
    object
        [ "orderId" .= show (orderId order)
        , "entityId" .= showEntityId (orderEntityId order)
        , "side" .= show (orderSide order)
        , "baseAsset" .= orderBaseAsset order
        , "quoteAsset" .= orderQuoteAsset order
        , "quantity" .= orderQuantity order
        , "limitPrice" .= orderLimitPrice order
        ]

renderInvalidOrder :: InvalidOrder -> Value
renderInvalidOrder invalidOrder =
    object
        [ "reason" .= show (invalidReason invalidOrder)
        , "order" .= renderOrder (invalidSubmittedOrder invalidOrder)
        ]

renderFill :: Fill -> Value
renderFill fill =
    object
        [ "buyOrderId" .= show (fillBuyOrderId fill)
        , "sellOrderId" .= show (fillSellOrderId fill)
        , "buyer" .= showEntityId (fillBuyer fill)
        , "seller" .= showEntityId (fillSeller fill)
        , "baseAsset" .= fillBaseAsset fill
        , "quoteAsset" .= fillQuoteAsset fill
        , "quantity" .= fillQuantity fill
        , "price" .= fillPrice fill
        ]

renderExpiredOrder :: ExpiredOrder -> Value
renderExpiredOrder expiredOrder =
    object
        [ "orderId" .= show (expiredOrderId expiredOrder)
        , "entityId" .= showEntityId (expiredEntityId expiredOrder)
        , "quantity" .= expiredQuantity expiredOrder
        ]

renderLotteryOffer :: LotteryOffer -> Value
renderLotteryOffer offer =
    object
        [ "assetId" .= lotteryOfferAssetId offer
        , "ticketPrice" .= lotteryOfferTicketPrice offer
        , "oddsNumerator" .= lotteryOfferOddsNumerator offer
        , "oddsDenominator" .= lotteryOfferOddsDenominator offer
        , "payoutQuantity" .= lotteryOfferPayoutQuantity offer
        ]

renderLotteryPurchase :: LotteryPurchase -> Value
renderLotteryPurchase purchase =
    object
        [ "entityId" .= showEntityId (lotteryPurchaseEntityId purchase)
        , "assetId" .= lotteryPurchaseAssetId purchase
        , "quantity" .= lotteryPurchaseQuantity purchase
        ]

renderLotteryIssuance :: LotteryIssuance -> Value
renderLotteryIssuance issuance =
    object
        [ "entityId" .= showEntityId (lotteryIssuanceEntityId issuance)
        , "seriesId" .= lotteryIssuanceSeriesId issuance
        , "ticketCount" .= lotteryIssuanceTicketCount issuance
        ]

renderLotterySettlement :: LotterySettlement -> Value
renderLotterySettlement settlement =
    object
        [ "entityId" .= showEntityId (lotterySettlementEntityId settlement)
        , "seriesId" .= lotterySettlementSeriesId settlement
        , "ticketCount" .= lotterySettlementTicketCount settlement
        , "winCount" .= lotterySettlementWinCount settlement
        , "payoutQuantity" .= lotterySettlementPayoutQuantity settlement
        ]

renderSurvivalResult :: SurvivalResult -> Value
renderSurvivalResult result =
    object
        [ "entityId" .= showEntityId (survivalEntityId result)
        , "paidStake" .= survivalPaidStake result
        , "refunded" .= survivalRefunded result
        ]

renderGrant :: Grant -> Value
renderGrant grant =
    object
        [ "entityId" .= showEntityId (grantEntityId grant)
        , "assetId" .= show (grantAssetId grant)
        , "quantity" .= grantQuantity grant
        ]

showEntityId :: EntityId -> String
showEntityId (EntityId entityId) = show entityId

showGameId :: GameId -> String
showGameId (GameId gameId) = show gameId

showMarketId :: MarketId -> String
showMarketId (MarketId marketIdValue) = show marketIdValue

showPlayerId :: PlayerId -> String
showPlayerId (PlayerId playerId) = show playerId
