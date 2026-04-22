module Tlon.Game.Default.Rules
  ( applyRedemptions,
    applySurvival,
    grantNextRoundTokens,
    redemptionForRound,
  )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Tlon.Core.Event
import Tlon.Core.Rng
import Tlon.Core.State
import Tlon.Core.Types

redemptionForRound :: Int -> Map AssetId Quantity
redemptionForRound roundNumber =
  case ((roundNumber - 1) `mod` 3) + 1 of
    1 -> Map.fromList [(TLN101, 1), (TLN102, 2), (TLN103, 0)]
    2 -> Map.fromList [(TLN101, 0), (TLN102, 1), (TLN103, 2)]
    _ -> Map.fromList [(TLN101, 2), (TLN102, 0), (TLN103, 1)]

applyRedemptions :: GameState -> (GameState, [Redemption])
applyRedemptions state =
  let governmentId = findGovernmentId state
      redemptionTable = gameRedemptionTable state
      (holdings', redemptions) =
        foldl
          (redeemEntity governmentId redemptionTable)
          (gameHoldings state, [])
          (livingMortals state)
   in (state {gameHoldings = holdings'}, reverse redemptions)

redeemEntity :: EntityId -> Map AssetId Quantity -> (Holdings, [Redemption]) -> EntityId -> (Holdings, [Redemption])
redeemEntity governmentId redemptionTable (holdings, events) entityId' =
  foldl
    (redeemOne governmentId redemptionTable entityId')
    (holdings, events)
    allAbstractAssets

redeemOne :: EntityId -> Map AssetId Quantity -> EntityId -> (Holdings, [Redemption]) -> AssetId -> (Holdings, [Redemption])
redeemOne governmentId redemptionTable entityId' (holdings, events) asset =
  let quantity = balanceOf holdings entityId' asset
      payoutRate = Map.findWithDefault 0 asset redemptionTable
      payout = quantity * payoutRate
   in if quantity <= 0
        then (holdings, events)
        else
          let holdings' =
                adjustBalance governmentId asset quantity
                  . adjustBalance entityId' asset (negate quantity)
                  . adjustBalance governmentId TLN001 (negate payout)
                  . adjustBalance entityId' TLN001 payout
                  $ holdings
           in (holdings', Redemption entityId' asset quantity payout : events)

applySurvival :: GameState -> (GameState, [SurvivalResult], Maybe EntityId)
applySurvival state =
  let governmentId = findGovernmentId state
      mortals = livingMortals state
      (stateAfterStake, eligible, paidSet) = foldl (takeStake governmentId) (state, [], []) mortals
      (stateAfterRefund, refundRecipient) = refundStake governmentId stateAfterStake eligible
      results =
        [ SurvivalResult
            { survivalEntityId = entityId',
              survivalPaidStake = entityId' `elem` paidSet,
              survivalRefunded = refundRecipient == Just entityId'
            }
          | entityId' <- mortals
        ]
   in (stateAfterRefund, results, refundRecipient)

takeStake :: EntityId -> (GameState, [EntityId], [EntityId]) -> EntityId -> (GameState, [EntityId], [EntityId])
takeStake governmentId (state, eligible, paidSet) entityId' =
  if balanceOf (gameHoldings state) entityId' TLN001 >= 1
    then
      let holdings' =
            adjustBalance governmentId TLN001 1
              . adjustBalance entityId' TLN001 (-1)
              $ gameHoldings state
       in (state {gameHoldings = holdings'}, entityId' : eligible, entityId' : paidSet)
    else
      let entities' = setEntityAlive entityId' False (gameEntities state)
       in (state {gameEntities = entities'}, eligible, paidSet)

refundStake :: EntityId -> GameState -> [EntityId] -> (GameState, Maybe EntityId)
refundStake governmentId state eligible =
  case eligible of
    [] -> (state, Nothing)
    _ ->
      let (index, seed') = drawBounded (length eligible) (gameSeed state)
          recipient = eligible !! index
          holdings' =
            adjustBalance governmentId TLN001 (-1)
              . adjustBalance recipient TLN001 1
              $ gameHoldings state
       in (state {gameHoldings = holdings', gameSeed = seed'}, Just recipient)

grantNextRoundTokens :: Quantity -> GameState -> (GameState, [Grant])
grantNextRoundTokens quantityPerEntity state =
  let governmentId = findGovernmentId state
      go (currentState, grants) entityId' =
        let (state', entityGrants) = grantOneEntity governmentId quantityPerEntity currentState entityId'
         in (state', grants ++ entityGrants)
      (state', grants') = foldl go (state, []) (livingMortals state)
   in (state', grants')

grantOneEntity :: EntityId -> Quantity -> GameState -> EntityId -> (GameState, [Grant])
grantOneEntity _ quantityPerEntity state _
  | quantityPerEntity <= 0 = (state, [])
grantOneEntity governmentId quantityPerEntity state entityId' =
  let grantStep (currentState, grants) _ =
        let (asset, seed') = drawFromList allAbstractAssets (gameSeed currentState)
            holdings' =
              adjustBalance governmentId asset (-1)
                . adjustBalance entityId' asset 1
                $ gameHoldings currentState
            currentState' = currentState {gameHoldings = holdings', gameSeed = seed'}
         in (currentState', grants ++ [Grant entityId' asset 1])
   in foldl grantStep (state, []) [1 .. quantityPerEntity]
