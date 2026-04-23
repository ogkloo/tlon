module Tlon.Game.Default.Rules
  ( applyLotteryPurchases,
    applyLotterySettlements,
    applySurvival,
    grantNextRoundTokens,
    lotteryMenuForRound,
  )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Tlon.Core.Event
import Tlon.Core.Rng
import Tlon.Core.State
import Tlon.Core.Types

lotteryMenuForRound :: Int -> [LotteryOffer]
lotteryMenuForRound roundNumber =
  case ((roundNumber - 1) `mod` 3) + 1 of
    1 ->
      [ LotteryOffer (assetSeriesId TLN101) 1 1 2 1 1
      , LotteryOffer (assetSeriesId TLN102) 1 1 3 2 2
      , LotteryOffer (assetSeriesId TLN103) 1 1 4 3 3
      ]
    2 ->
      [ LotteryOffer (assetSeriesId TLN101) 1 1 4 2 2
      , LotteryOffer (assetSeriesId TLN102) 1 1 2 1 1
      , LotteryOffer (assetSeriesId TLN103) 1 1 3 2 2
      ]
    _ ->
      [ LotteryOffer (assetSeriesId TLN101) 1 1 3 2 2
      , LotteryOffer (assetSeriesId TLN102) 1 1 4 3 3
      , LotteryOffer (assetSeriesId TLN103) 1 1 2 1 1
      ]

applyLotteryPurchases :: [LotteryPurchase] -> GameState -> (GameState, [LotteryIssuance])
applyLotteryPurchases purchases state =
  let normalizedPurchases = normalizePurchases state purchases
   in case normalizedPurchases of
        [] -> (state, [])
        _ ->
          let governmentId = findGovernmentId state
              offersByAsset = Map.fromList [(lotteryOfferAssetId offer, offer) | offer <- gameLotteryMenu state]
              (holdings', seriesCatalog', issuances) =
                foldl'
                  (issuePurchase state governmentId offersByAsset)
                  (gameHoldings state, gameSeriesCatalog state, [])
                  normalizedPurchases
           in (state {gameHoldings = holdings', gameSeriesCatalog = seriesCatalog'}, reverse issuances)

applyLotterySettlements :: GameState -> (GameState, [LotterySettlement])
applyLotterySettlements state =
  let governmentId = findGovernmentId state
      roundNumber = gameRoundNumber state
      maturities =
        [ series
        | series <- Map.elems (gameSeriesCatalog state)
        , instrumentSeriesKind series == LotteryTicketSeries
        , instrumentSeriesSettlementRound series == Just roundNumber
        ]
      (holdings', settlements, seed') =
        foldl'
          (settleSeries governmentId)
          (gameHoldings state, [], gameSeed state)
          maturities
   in (state {gameHoldings = holdings', gameSeed = seed'}, reverse settlements)

normalizePurchases :: GameState -> [LotteryPurchase] -> [LotteryPurchase]
normalizePurchases state =
  foldl' collect []
  where
    offerAssets = [lotteryOfferAssetId offer | offer <- gameLotteryMenu state]
    collect purchases purchase
      | lotteryPurchaseQuantity purchase <= 0 = purchases
      | lotteryPurchaseAssetId purchase `notElem` offerAssets = purchases
      | otherwise = purchases ++ [purchase]

issuePurchase ::
  GameState ->
  EntityId ->
  Map SeriesId LotteryOffer ->
  (Holdings, SeriesCatalog, [LotteryIssuance]) ->
  LotteryPurchase ->
  (Holdings, SeriesCatalog, [LotteryIssuance])
issuePurchase state governmentId offersByAsset (holdings, seriesCatalog, issuances) purchase =
  case Map.lookup (lotteryPurchaseAssetId purchase) offersByAsset of
    Nothing -> (holdings, seriesCatalog, issuances)
    Just offer ->
      let entityId' = lotteryPurchaseEntityId purchase
          requestedTickets = lotteryPurchaseQuantity purchase
          ticketPrice = lotteryOfferTicketPrice offer
          affordableTickets =
            if ticketPrice <= 0
              then 0
              else balanceOf holdings entityId' (assetSeriesId TLN001) `div` ticketPrice
          ticketCount = min requestedTickets affordableTickets
       in if ticketCount <= 0
            then (holdings, seriesCatalog, issuances)
            else
              case assetIdForSeries state (lotteryOfferAssetId offer) of
                Nothing -> (holdings, seriesCatalog, issuances)
                Just payoutAsset ->
                  let ticketSeries = ticketSeriesForOffer governmentId (gameRoundNumber state) payoutAsset offer
                      ticketSeriesIdValue = instrumentSeriesId ticketSeries
                      purchaseCost = ticketCount * ticketPrice
                      holdings' =
                        adjustBalance governmentId (assetSeriesId TLN001) purchaseCost
                          . adjustBalance entityId' (assetSeriesId TLN001) (negate purchaseCost)
                          . adjustBalance governmentId ticketSeriesIdValue (negate ticketCount)
                          . adjustBalance entityId' ticketSeriesIdValue ticketCount
                          $ holdings
                      seriesCatalog' = Map.insert ticketSeriesIdValue ticketSeries seriesCatalog
                      issuance =
                        LotteryIssuance
                          { lotteryIssuanceEntityId = entityId'
                          , lotteryIssuanceSeriesId = ticketSeriesIdValue
                          , lotteryIssuanceTicketCount = ticketCount
                          }
                   in (holdings', seriesCatalog', issuance : issuances)

settleSeries :: EntityId -> (Holdings, [LotterySettlement], Int) -> InstrumentSeries -> (Holdings, [LotterySettlement], Int)
settleSeries governmentId (holdings, settlements, seed) series =
  let ticketSeriesIdValue = instrumentSeriesId series
      payoutAsset = instrumentSeriesPayoutAssetId series
      oddsNumerator = instrumentSeriesOddsNumerator series
      oddsDenominator = instrumentSeriesOddsDenominator series
      payoutQuantity = instrumentSeriesPayoutQuantity series
      holders =
        [ (entityId', balanceOf holdings entityId' ticketSeriesIdValue)
        | (entityId', _) <- Map.toList holdings
        , balanceOf holdings entityId' ticketSeriesIdValue > 0
        ]
   in foldl'
        (settleHolder governmentId ticketSeriesIdValue payoutAsset oddsNumerator oddsDenominator payoutQuantity)
        (holdings, settlements, seed)
        holders

settleHolder ::
  EntityId ->
  SeriesId ->
  Maybe AssetId ->
  Maybe Int ->
  Maybe Int ->
  Maybe Quantity ->
  (Holdings, [LotterySettlement], Int) ->
  (EntityId, Quantity) ->
  (Holdings, [LotterySettlement], Int)
settleHolder governmentId ticketSeriesIdValue payoutAsset oddsNumerator oddsDenominator payoutQuantity (holdings, settlements, seed) (entityId', ticketCount) =
  case (payoutAsset, oddsNumerator, oddsDenominator, payoutQuantity) of
    (Just payoutAssetId, Just numerator, Just denominator, Just payoutPerWin) ->
      let (winCount, seed') = drawWins ticketCount numerator denominator seed
          totalPayout = winCount * payoutPerWin
          holdings' =
            adjustBalance entityId' ticketSeriesIdValue (negate ticketCount)
              . adjustBalance governmentId ticketSeriesIdValue ticketCount
              . adjustBalance governmentId (assetSeriesId payoutAssetId) (negate totalPayout)
              . adjustBalance entityId' (assetSeriesId payoutAssetId) totalPayout
              $ holdings
          settlement =
            LotterySettlement
              { lotterySettlementEntityId = entityId'
              , lotterySettlementSeriesId = ticketSeriesIdValue
              , lotterySettlementTicketCount = ticketCount
              , lotterySettlementWinCount = winCount
              , lotterySettlementPayoutQuantity = totalPayout
              }
       in (holdings', settlement : settlements, seed')
    _ -> (holdings, settlements, seed)

ticketSeriesForOffer :: EntityId -> Int -> AssetId -> LotteryOffer -> InstrumentSeries
ticketSeriesForOffer issuerId roundIssued payoutAsset offer =
  let duration = max 1 (lotteryOfferDurationRounds offer)
      settlementRound = roundIssued + duration
      ticketSeriesIdValue =
        ticketSeriesId
          roundIssued
          duration
          payoutAsset
          (lotteryOfferTicketPrice offer)
          (lotteryOfferOddsNumerator offer)
          (lotteryOfferOddsDenominator offer)
          (lotteryOfferPayoutQuantity offer)
   in InstrumentSeries
        { instrumentSeriesId = ticketSeriesIdValue
        , instrumentSeriesBaseAssetId = Nothing
        , instrumentSeriesKind = LotteryTicketSeries
        , instrumentSeriesIssuer = issuerId
        , instrumentSeriesRoundIssued = Just roundIssued
        , instrumentSeriesSettlementRound = Just settlementRound
        , instrumentSeriesTicketPrice = Just (lotteryOfferTicketPrice offer)
        , instrumentSeriesOddsNumerator = Just (lotteryOfferOddsNumerator offer)
        , instrumentSeriesOddsDenominator = Just (lotteryOfferOddsDenominator offer)
        , instrumentSeriesPayoutQuantity = Just (lotteryOfferPayoutQuantity offer)
        , instrumentSeriesPayoutAssetId = Just payoutAsset
        }

drawWins :: Quantity -> Int -> Int -> Int -> (Quantity, Int)
drawWins ticketCount numerator denominator seed =
  let go 0 wins currentSeed = (wins, currentSeed)
      go remaining wins currentSeed =
        let (draw, nextSeed') = drawBounded denominator currentSeed
            wins' = if draw < numerator then wins + 1 else wins
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
  if balanceOf (gameHoldings state) entityId' (assetSeriesId TLN001) >= 1
    then
      let holdings' =
            adjustBalance governmentId (assetSeriesId TLN001) 1
              . adjustBalance entityId' (assetSeriesId TLN001) (-1)
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
            adjustBalance governmentId (assetSeriesId TLN001) (-1)
              . adjustBalance recipient (assetSeriesId TLN001) 1
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
              adjustBalance governmentId (assetSeriesId asset) (-1)
                . adjustBalance entityId' (assetSeriesId asset) 1
                $ gameHoldings currentState
            currentState' = currentState {gameHoldings = holdings', gameSeed = seed'}
         in (currentState', grants ++ [Grant entityId' asset 1])
   in foldl grantStep (state, []) [1 .. quantityPerEntity]
