module Tlon.Game.Default.Rules
  ( applyLotteryPurchases,
    applySurvival,
    grantNextRoundTokens,
    lotteryMenuForRound,
  )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl')
import Tlon.Core.Event
import Tlon.Core.Rng
import Tlon.Core.State
import Tlon.Core.Types

lotteryMenuForRound :: Int -> [LotteryOffer]
lotteryMenuForRound roundNumber =
  case ((roundNumber - 1) `mod` 3) + 1 of
    1 ->
      [ LotteryOffer TLN101 1 1 2 1
      , LotteryOffer TLN102 1 1 3 2
      , LotteryOffer TLN103 1 1 4 3
      ]
    2 ->
      [ LotteryOffer TLN101 1 1 4 2
      , LotteryOffer TLN102 1 1 2 1
      , LotteryOffer TLN103 1 1 3 2
      ]
    _ ->
      [ LotteryOffer TLN101 1 1 3 2
      , LotteryOffer TLN102 1 1 4 3
      , LotteryOffer TLN103 1 1 2 1
      ]

applyLotteryPurchases :: [LotteryPurchase] -> GameState -> (GameState, [LotteryResult])
applyLotteryPurchases purchases state =
  let normalizedPurchases = normalizePurchases state purchases
   in case normalizedPurchases of
        [] -> (state, [])
        _ ->
          let governmentId = findGovernmentId state
              offersByAsset = Map.fromList [(lotteryOfferAssetId offer, offer) | offer <- gameLotteryMenu state]
              (holdings', results, seed') =
                foldl'
                  (resolvePurchase governmentId offersByAsset)
                  (gameHoldings state, [], gameSeed state)
                  normalizedPurchases
           in (state {gameHoldings = holdings', gameSeed = seed'}, reverse results)

normalizePurchases :: GameState -> [LotteryPurchase] -> [LotteryPurchase]
normalizePurchases state =
  foldl' collect []
  where
    offerAssets = [lotteryOfferAssetId offer | offer <- gameLotteryMenu state]
    collect purchases purchase
      | lotteryPurchaseQuantity purchase <= 0 = purchases
      | lotteryPurchaseAssetId purchase `notElem` offerAssets = purchases
      | otherwise = purchases ++ [purchase]

resolvePurchase ::
  EntityId ->
  Map AssetId LotteryOffer ->
  (Holdings, [LotteryResult], Int) ->
  LotteryPurchase ->
  (Holdings, [LotteryResult], Int)
resolvePurchase governmentId offersByAsset (holdings, results, seed) purchase =
  case Map.lookup (lotteryPurchaseAssetId purchase) offersByAsset of
    Nothing -> (holdings, results, seed)
    Just offer ->
      let entityId' = lotteryPurchaseEntityId purchase
          requestedTickets = lotteryPurchaseQuantity purchase
          ticketPrice = lotteryOfferTicketPrice offer
          affordableTickets =
            if ticketPrice <= 0
              then 0
              else balanceOf holdings entityId' TLN001 `div` ticketPrice
          ticketCount = min requestedTickets affordableTickets
       in if ticketCount <= 0
            then (holdings, results, seed)
            else
              let purchaseCost = ticketCount * ticketPrice
                  holdingsAfterPurchase =
                    adjustBalance governmentId TLN001 purchaseCost
                      . adjustBalance entityId' TLN001 (negate purchaseCost)
                      $ holdings
                  (winCount, seed') = drawWins ticketCount offer seed
                  payoutQuantity = winCount * lotteryOfferPayoutQuantity offer
                  holdingsAfterPayout =
                    adjustBalance governmentId (lotteryOfferAssetId offer) (negate payoutQuantity)
                      . adjustBalance entityId' (lotteryOfferAssetId offer) payoutQuantity
                      $ holdingsAfterPurchase
                  result =
                    LotteryResult
                      { lotteryResultEntityId = entityId'
                      , lotteryResultAssetId = lotteryOfferAssetId offer
                      , lotteryResultTicketCount = ticketCount
                      , lotteryResultWinCount = winCount
                      , lotteryResultPayoutQuantity = payoutQuantity
                      }
               in (holdingsAfterPayout, result : results, seed')

drawWins :: Quantity -> LotteryOffer -> Int -> (Quantity, Int)
drawWins ticketCount offer seed =
  let go 0 wins currentSeed = (wins, currentSeed)
      go remaining wins currentSeed =
        let (draw, nextSeed') = drawBounded (lotteryOfferOddsDenominator offer) currentSeed
            wins' = if draw < lotteryOfferOddsNumerator offer then wins + 1 else wins
         in go (remaining - 1) wins' nextSeed'
   in go ticketCount 0 seed

applySurvival :: GameState -> (GameState, [SurvivalResult], Maybe EntityId)
applySurvival state =
  let governmentId = findGovernmentId state
      mortals = livingMortals state
      (stateAfterStake, eligible, paidSet) = foldl (takeStake governmentId) (state, [], []) mortals
      (stateAfterRefund, refundRecipient) = refundStake governmentId stateAfterStake eligible
      results =
        [ SurvivalResult
            { survivalEntityId = entityId'
            , survivalPaidStake = entityId' `elem` paidSet
            , survivalRefunded = refundRecipient == Just entityId'
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
