module Tlon.Game.Default.Rules (
    applyOfferingPurchases,
    applyLotterySettlements,
    defaultActorInputs,
    defaultOfferingsForRound,
    defaultRoundEffects,
    defaultRoundRules,
    grantNextRoundTokens,
)
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Tlon.Core.Event
import Tlon.Core.Rng
import Tlon.Core.Rules
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Game.Default.Config

defaultRoundRules :: DefaultConfig -> RoundRules
defaultRoundRules config =
    RoundRules
        { roundRulesApplyOfferingPurchases = applyOfferingPurchases
        , roundRulesApplyInstrumentSettlements = applyLotterySettlements
        , roundRulesApplyRoundEffects = defaultRoundEffects
        , roundRulesGrantNextRound = grantNextRoundTokens (configRoundGrantQuantity config)
        , roundRulesActiveOfferingsForRound = \state roundNumber -> defaultOfferingsForRound (findGovernmentId state) roundNumber
        }

defaultOfferingsForRound :: EntityId -> Int -> [InstrumentOffering]
defaultOfferingsForRound issuerId roundNumber =
    case ((roundNumber - 1) `mod` 3) + 1 of
        1 ->
            [ lotteryOffering issuerId roundNumber (assetSeriesId TLN101) 1 1 2 1 1
            , lotteryOffering issuerId roundNumber (assetSeriesId TLN102) 1 1 3 2 2
            , lotteryOffering issuerId roundNumber (assetSeriesId TLN103) 1 1 4 3 3
            ]
        2 ->
            [ lotteryOffering issuerId roundNumber (assetSeriesId TLN101) 1 1 4 2 2
            , lotteryOffering issuerId roundNumber (assetSeriesId TLN102) 1 1 2 1 1
            , lotteryOffering issuerId roundNumber (assetSeriesId TLN103) 1 1 3 2 2
            ]
        _ ->
            [ lotteryOffering issuerId roundNumber (assetSeriesId TLN101) 1 1 3 2 2
            , lotteryOffering issuerId roundNumber (assetSeriesId TLN102) 1 1 4 3 3
            , lotteryOffering issuerId roundNumber (assetSeriesId TLN103) 1 1 2 1 1
            ]

lotteryOffering :: EntityId -> Int -> SeriesId -> Quantity -> Int -> Int -> Quantity -> Int -> InstrumentOffering
lotteryOffering issuerId roundNumber payoutSeriesId ticketPrice oddsNumerator oddsDenominator payoutQuantity durationRounds =
    let duration = max 1 durationRounds
        seriesId = ticketSeriesId roundNumber duration payoutSeriesId ticketPrice oddsNumerator oddsDenominator payoutQuantity
     in InstrumentOffering
            { instrumentOfferingSeriesId = seriesId
            , instrumentOfferingIssuer = issuerId
            , instrumentOfferingTerms =
                LotteryOffering
                    LotteryOfferingTerms
                        { lotteryOfferingPayoutSeriesId = payoutSeriesId
                        , lotteryOfferingTicketPrice = ticketPrice
                        , lotteryOfferingOddsNumerator = oddsNumerator
                        , lotteryOfferingOddsDenominator = oddsDenominator
                        , lotteryOfferingPayoutQuantity = payoutQuantity
                        , lotteryOfferingDurationRounds = duration
                        }
            }

applyOfferingPurchases :: [OfferingPurchase] -> GameState -> (GameState, [LotteryIssuance])
applyOfferingPurchases purchases state =
    let normalizedPurchases = normalizePurchases state purchases
     in case normalizedPurchases of
            [] -> (state, [])
            _ ->
                let offeringsBySeries = Map.fromList [(instrumentOfferingSeriesId offering, offering) | offering <- gameActiveOfferings state]
                    (holdings', seriesCatalog', markets', issuances) =
                        foldl'
                            (issuePurchase state offeringsBySeries)
                            (gameHoldings state, gameSeriesCatalog state, gameMarkets state, [])
                            normalizedPurchases
                 in (state{gameHoldings = holdings', gameSeriesCatalog = seriesCatalog', gameMarkets = markets'}, reverse issuances)

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
     in (state{gameHoldings = holdings', gameSeed = seed'}, reverse settlements)

normalizePurchases :: GameState -> [OfferingPurchase] -> [OfferingPurchase]
normalizePurchases state =
    foldl' collect []
  where
    offeredSeries = [instrumentOfferingSeriesId offering | offering <- gameActiveOfferings state]
    collect purchases purchase
        | offeringPurchaseQuantity purchase <= 0 = purchases
        | offeringPurchaseSeriesId purchase `notElem` offeredSeries = purchases
        | otherwise = purchases ++ [purchase]

issuePurchase ::
    GameState ->
    Map SeriesId InstrumentOffering ->
    (Holdings, SeriesCatalog, Map MarketId Market, [LotteryIssuance]) ->
    OfferingPurchase ->
    (Holdings, SeriesCatalog, Map MarketId Market, [LotteryIssuance])
issuePurchase state offeringsBySeries (holdings, seriesCatalog, markets, issuances) purchase =
    case Map.lookup (offeringPurchaseSeriesId purchase) offeringsBySeries of
        Nothing -> (holdings, seriesCatalog, markets, issuances)
        Just offering ->
            case instrumentOfferingTerms offering of
                LotteryOffering terms ->
                    issueLotteryPurchase state terms offering (holdings, seriesCatalog, markets, issuances) purchase

issueLotteryPurchase ::
    GameState ->
    LotteryOfferingTerms ->
    InstrumentOffering ->
    (Holdings, SeriesCatalog, Map MarketId Market, [LotteryIssuance]) ->
    OfferingPurchase ->
    (Holdings, SeriesCatalog, Map MarketId Market, [LotteryIssuance])
issueLotteryPurchase state terms offering (holdings, seriesCatalog, markets, issuances) purchase =
    let entityId' = offeringPurchaseEntityId purchase
        requestedTickets = offeringPurchaseQuantity purchase
        ticketPrice = lotteryOfferingTicketPrice terms
        affordableTickets =
            if ticketPrice <= 0
                then 0
                else balanceOf holdings entityId' (assetSeriesId TLN001) `div` ticketPrice
        ticketCount = min requestedTickets affordableTickets
     in if ticketCount <= 0
            then (holdings, seriesCatalog, markets, issuances)
            else
                let ticketSeries = ticketSeriesForOffering state terms offering
                    ticketSeriesIdValue = instrumentSeriesId ticketSeries
                    purchaseCost = ticketCount * ticketPrice
                    holdings' =
                        adjustBalance (instrumentOfferingIssuer offering) (assetSeriesId TLN001) purchaseCost
                            . adjustBalance entityId' (assetSeriesId TLN001) (negate purchaseCost)
                            . adjustBalance (instrumentOfferingIssuer offering) ticketSeriesIdValue (negate ticketCount)
                            . adjustBalance entityId' ticketSeriesIdValue ticketCount
                            $ holdings
                    seriesCatalog' = Map.insert ticketSeriesIdValue ticketSeries seriesCatalog
                    markets' = listTicketSeries ticketSeriesIdValue markets
                    issuance =
                        LotteryIssuance
                            { lotteryIssuanceEntityId = entityId'
                            , lotteryIssuanceSeriesId = ticketSeriesIdValue
                            , lotteryIssuanceTicketCount = ticketCount
                            }
                 in (holdings', seriesCatalog', markets', issuance : issuances)

listTicketSeries :: SeriesId -> Map MarketId Market -> Map MarketId Market
listTicketSeries ticketSeriesIdValue =
    Map.map listOnMarket
  where
    pair = (ticketSeriesIdValue, assetSeriesId TLN001)
    listOnMarket market
        | pair `elem` marketPairs market = market
        | otherwise = market{marketPairs = marketPairs market ++ [pair]}

settleSeries :: EntityId -> (Holdings, [LotterySettlement], Int) -> InstrumentSeries -> (Holdings, [LotterySettlement], Int)
settleSeries governmentId (holdings, settlements, seed) series =
    case instrumentSeriesTerms series of
        LotteryInstrumentTerms terms ->
            let ticketSeriesIdValue = instrumentSeriesId series
                holders =
                    [ (entityId', balanceOf holdings entityId' ticketSeriesIdValue)
                    | (entityId', _) <- Map.toList holdings
                    , balanceOf holdings entityId' ticketSeriesIdValue > 0
                    ]
             in foldl'
                    (settleHolder governmentId ticketSeriesIdValue terms)
                    (holdings, settlements, seed)
                    holders
        _ -> (holdings, settlements, seed)

settleHolder ::
    EntityId ->
    SeriesId ->
    LotteryTerms ->
    (Holdings, [LotterySettlement], Int) ->
    (EntityId, Quantity) ->
    (Holdings, [LotterySettlement], Int)
settleHolder governmentId ticketSeriesIdValue terms (holdings, settlements, seed) (entityId', ticketCount) =
    let numerator = lotteryTermsOddsNumerator terms
        denominator = lotteryTermsOddsDenominator terms
        payoutPerWin = lotteryTermsPayoutQuantity terms
        payoutSeriesId = lotteryTermsPayoutSeriesId terms
        (winCount, seed') = drawWins ticketCount numerator denominator seed
        totalPayout = winCount * payoutPerWin
        holdings' =
            adjustBalance entityId' ticketSeriesIdValue (negate ticketCount)
                . adjustBalance governmentId ticketSeriesIdValue ticketCount
                . adjustBalance governmentId payoutSeriesId (negate totalPayout)
                . adjustBalance entityId' payoutSeriesId totalPayout
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

ticketSeriesForOffering :: GameState -> LotteryOfferingTerms -> InstrumentOffering -> InstrumentSeries
ticketSeriesForOffering state terms offering =
    let roundIssued = gameRoundNumber state
        duration = max 1 (lotteryOfferingDurationRounds terms)
        settlementRound = roundIssued + duration
        ticketSeriesIdValue = instrumentOfferingSeriesId offering
     in InstrumentSeries
            { instrumentSeriesId = ticketSeriesIdValue
            , instrumentSeriesKind = LotteryTicketSeries
            , instrumentSeriesIssuer = instrumentOfferingIssuer offering
            , instrumentSeriesRoundIssued = Just roundIssued
            , instrumentSeriesSettlementRound = Just settlementRound
            , instrumentSeriesTerms =
                LotteryInstrumentTerms
                    LotteryTerms
                        { lotteryTermsTicketPrice = lotteryOfferingTicketPrice terms
                        , lotteryTermsOddsNumerator = lotteryOfferingOddsNumerator terms
                        , lotteryTermsOddsDenominator = lotteryOfferingOddsDenominator terms
                        , lotteryTermsPayoutQuantity = lotteryOfferingPayoutQuantity terms
                        , lotteryTermsPayoutSeriesId = lotteryOfferingPayoutSeriesId terms
                        }
            }

drawWins :: Quantity -> Int -> Int -> Int -> (Quantity, Int)
drawWins ticketCount numerator denominator seed =
    let go 0 wins currentSeed = (wins, currentSeed)
        go remaining wins currentSeed =
            let (draw, nextSeed') = drawBounded denominator currentSeed
                wins' = if draw < numerator then wins + 1 else wins
             in go (remaining - 1) wins' nextSeed'
     in go ticketCount 0 seed

defaultRoundEffects :: GameState -> (GameState, [SurvivalResult], Maybe EntityId)
defaultRoundEffects state = (state, [], Nothing)

defaultActorInputs :: [EntityId] -> GameState -> RoundInputs
defaultActorInputs npcEntityIds state =
    RoundInputs
        { roundOrders = governmentMarketMakerOrders state ++ npcTraderOrders npcEntityIds state
        , roundOfferingPurchases = npcOfferingPurchases npcEntityIds state
        }

governmentMarketMakerOrders :: GameState -> [Order]
governmentMarketMakerOrders state =
    concatMap ordersForPair listedPairs
  where
    governmentId = findGovernmentId state
    listedPairs = take 8 (listedTlnPairs state)
    ordersForPair (index, marketId', baseSeries, quoteSeries) =
        let price = quotePrice state baseSeries
            buyOrder = Order (actorOrderId 1 index) governmentId marketId' Buy baseSeries quoteSeries 1 price
            sellOrder = Order (actorOrderId 2 index) governmentId marketId' Sell baseSeries quoteSeries 1 (price + 1)
         in filter (canFundOrder state) [buyOrder, sellOrder]

npcTraderOrders :: [EntityId] -> GameState -> [Order]
npcTraderOrders npcEntityIds state =
    [ order
    | (npcIndex, entityId') <- zip [1 ..] npcEntityIds
    , let pairs = listedTlnPairs state
    , not (null pairs)
    , let (pairIndex, _) = drawBounded (length pairs) (gameSeed state + npcIndex + gameRoundNumber state)
    , let (_, marketId', baseSeries, quoteSeries) = pairs !! pairIndex
    , let side = if even (gameSeed state + npcIndex) then Buy else Sell
    , let price = quotePrice state baseSeries
    , let order = Order (actorOrderId 10 npcIndex) entityId' marketId' side baseSeries quoteSeries 1 price
    ]

npcOfferingPurchases :: [EntityId] -> GameState -> [OfferingPurchase]
npcOfferingPurchases npcEntityIds state =
    [ OfferingPurchase entityId' (instrumentOfferingSeriesId offering) 1
    | (npcIndex, entityId') <- zip [1 ..] npcEntityIds
    , not (null (gameActiveOfferings state))
    , balanceOf (gameHoldings state) entityId' (assetSeriesId TLN001) > 0
    , let (offeringIndex, _) = drawBounded (length (gameActiveOfferings state)) (gameSeed state + npcIndex + 100)
    , let offering = gameActiveOfferings state !! offeringIndex
    ]

listedTlnPairs :: GameState -> [(Int, MarketId, SeriesId, SeriesId)]
listedTlnPairs state =
    zipWith
        indexedPair
        [1 ..]
        [ (marketId market, baseSeries, quoteSeries)
        | market <- Map.elems (gameMarkets state)
        , (baseSeries, quoteSeries) <- marketPairs market
        , quoteSeries == assetSeriesId TLN001
        ]
  where
    indexedPair index (marketId', baseSeries, quoteSeries) = (index, marketId', baseSeries, quoteSeries)

quotePrice :: GameState -> SeriesId -> Quantity
quotePrice state seriesId =
    case Map.lookup seriesId (gameSeriesCatalog state) of
        Just series -> max 1 (instrumentValueEstimate series)
        Nothing -> 1

instrumentValueEstimate :: InstrumentSeries -> Quantity
instrumentValueEstimate series =
    case instrumentSeriesTerms series of
        BaseInstrumentTerms assetId -> baseAssetQuote assetId
        LotteryInstrumentTerms terms ->
            max 1 ((lotteryTermsOddsNumerator terms * lotteryTermsPayoutQuantity terms) `div` max 1 (lotteryTermsOddsDenominator terms))
        RaffleInstrumentTerms -> 1
        DerivativeInstrumentTerms -> 1

canFundOrder :: GameState -> Order -> Bool
canFundOrder state order =
    case orderSide order of
        Buy -> balanceOf (gameHoldings state) (orderEntityId order) (orderQuoteAsset order) >= orderQuantity order * orderLimitPrice order
        Sell -> balanceOf (gameHoldings state) (orderEntityId order) (orderBaseAsset order) >= orderQuantity order

baseAssetQuote :: AssetId -> Quantity
baseAssetQuote assetId =
    case assetId of
        TLN001 -> 1
        TLN101 -> 2
        TLN102 -> 3
        TLN103 -> 4

actorOrderId :: Int -> Int -> OrderId
actorOrderId family index =
    OrderId (900000 + family * 1000 + index)

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
                currentState' = currentState{gameHoldings = holdings', gameSeed = seed'}
             in (currentState', grants ++ [Grant entityId' asset 1])
     in foldl grantStep (state, []) [1 .. quantityPerEntity]
