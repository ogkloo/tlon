{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.Api
  ( gameJson,
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
   in
    object
      [ "gameId" .= showGameId (runningGameId runningGame),
        "started" .= runningStarted runningGame,
        "seatCount" .= runningSeatCount runningGame,
        "humanSeatCount" .= humanSeatCount runningGame,
        "npcCount" .= runningNpcCount runningGame,
        "participants" .= map renderHumanPlayer (runningParticipants runningGame),
        "roundNumber" .= gameRoundNumber state,
        "winner" .= fmap showEntityId (gameWinner state),
        "redemptionTable" .= map renderAssetQuantity (Map.toList (gameRedemptionTable state)),
        "entities" .= map (entitySummary state) (Map.elems (gameEntities state))
      ]

reportJson :: RoundReport -> Value
reportJson report =
  object
    [ "roundNumber" .= reportRoundNumber report,
      "submittedOrders" .= map renderOrder (reportSubmittedOrders report),
      "invalidOrders" .= map renderInvalidOrder (reportInvalidOrders report),
      "fills" .= map renderFill (reportFills report),
      "expiredOrders" .= map renderExpiredOrder (reportExpiredOrders report),
      "redemptions" .= map renderRedemption (reportRedemptions report),
      "survivalResults" .= map renderSurvivalResult (reportSurvivalResults report),
      "refundRecipient" .= fmap showEntityId (reportRefundRecipient report),
      "nextRoundGrants" .= map renderGrant (reportNextRoundGrants report),
      "nextRedemptionTable" .= map renderAssetQuantity (Map.toList (reportNextRedemptionTable report))
    ]

entitySummary :: GameState -> Entity -> Value
entitySummary state entity =
  object
    [ "entityId" .= showEntityId (entityId entity),
      "name" .= entityName entity,
      "kind" .= show (entityKind entity),
      "alive" .= entityAlive entity,
      "holdings" .= map renderAssetQuantity (Map.toList (Map.findWithDefault Map.empty (entityId entity) (gameHoldings state)))
    ]

renderHumanPlayer :: HumanPlayer -> Value
renderHumanPlayer participant =
  object
    [ "playerId" .= showPlayerId (humanPlayerId participant),
      "name" .= humanPlayerName participant,
      "entityId" .= fmap showEntityId (humanPlayerEntityId participant)
    ]

renderAssetQuantity :: (AssetId, Quantity) -> Value
renderAssetQuantity (assetId, quantity) =
  object
    [ "assetId" .= show assetId,
      "quantity" .= quantity
    ]

renderOrder :: Order -> Value
renderOrder order =
  object
    [ "orderId" .= show (orderId order),
      "entityId" .= showEntityId (orderEntityId order),
      "side" .= show (orderSide order),
      "baseAsset" .= show (orderBaseAsset order),
      "quoteAsset" .= show (orderQuoteAsset order),
      "quantity" .= orderQuantity order,
      "limitPrice" .= orderLimitPrice order
    ]

renderInvalidOrder :: InvalidOrder -> Value
renderInvalidOrder invalidOrder =
  object
    [ "reason" .= show (invalidReason invalidOrder),
      "order" .= renderOrder (invalidSubmittedOrder invalidOrder)
    ]

renderFill :: Fill -> Value
renderFill fill =
  object
    [ "buyOrderId" .= show (fillBuyOrderId fill),
      "sellOrderId" .= show (fillSellOrderId fill),
      "buyer" .= showEntityId (fillBuyer fill),
      "seller" .= showEntityId (fillSeller fill),
      "baseAsset" .= show (fillBaseAsset fill),
      "quoteAsset" .= show (fillQuoteAsset fill),
      "quantity" .= fillQuantity fill,
      "price" .= fillPrice fill
    ]

renderExpiredOrder :: ExpiredOrder -> Value
renderExpiredOrder expiredOrder =
  object
    [ "orderId" .= show (expiredOrderId expiredOrder),
      "entityId" .= showEntityId (expiredEntityId expiredOrder),
      "quantity" .= expiredQuantity expiredOrder
    ]

renderRedemption :: Redemption -> Value
renderRedemption redemption =
  object
    [ "entityId" .= showEntityId (redemptionEntityId redemption),
      "assetId" .= show (redemptionAssetId redemption),
      "quantity" .= redemptionQuantity redemption,
      "payout" .= redemptionPayout redemption
    ]

renderSurvivalResult :: SurvivalResult -> Value
renderSurvivalResult result =
  object
    [ "entityId" .= showEntityId (survivalEntityId result),
      "paidStake" .= survivalPaidStake result,
      "refunded" .= survivalRefunded result
    ]

renderGrant :: Grant -> Value
renderGrant grant =
  object
    [ "entityId" .= showEntityId (grantEntityId grant),
      "assetId" .= show (grantAssetId grant),
      "quantity" .= grantQuantity grant
    ]

showEntityId :: EntityId -> String
showEntityId (EntityId entityId) = show entityId

showGameId :: GameId -> String
showGameId (GameId gameId) = show gameId

showPlayerId :: PlayerId -> String
showPlayerId (PlayerId playerId) = show playerId
