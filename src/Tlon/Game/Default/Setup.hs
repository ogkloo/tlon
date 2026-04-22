module Tlon.Game.Default.Setup (
    initialState,
)
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Tlon.Core.Rng
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Game.Default.Config
import Tlon.Game.Default.Rules

initialState :: DefaultConfig -> GameState
initialState config =
    let government = Entity (EntityId 0) "Government" GovernmentEntity True
        (players, seedAfterNames) = buildPlayers (configPlayerCount config) (configInitialSeed config)
        allEntities = government : players
        entitiesMap = Map.fromList [(entityId entity, entity) | entity <- allEntities]
        assetIssuers =
            Map.fromList
                [ (TLN001, entityId government)
                , (TLN101, entityId government)
                , (TLN102, entityId government)
                , (TLN103, entityId government)
                ]
        seriesCatalog = buildBaseSeriesCatalog assetIssuers
        market =
            Market
                { marketId = MarketId 1
                , marketName = "Default Market"
                , marketOwner = entityId government
                , marketPairs = [(asset, TLN001) | asset <- allAbstractAssets]
                , marketRules = [QuoteAssetMustBeOwnerIssuedCurrency]
                }
        (holdings, finalSeed) =
            buildInitialHoldings
                config
                (entityId government)
                players
                seedAfterNames
        roundNumber = 1
     in GameState
            { gameRoundNumber = roundNumber
            , gameMatchingPolicy = configMatchingPolicy config
            , gameSeed = finalSeed
            , gameEntities = entitiesMap
            , gameAssetIssuers = assetIssuers
            , gameSeriesCatalog = seriesCatalog
            , gameMarkets = Map.fromList [(marketId market, market)]
            , gameHoldings = holdings
            , gameLotteryMenu = lotteryMenuForRound roundNumber
            , gamePreviousReport = Nothing
            , gameWinner = Nothing
            }

buildBaseSeriesCatalog :: Map AssetId EntityId -> SeriesCatalog
buildBaseSeriesCatalog assetIssuers =
    Map.fromList
        [ ( assetSeriesId assetId
          , InstrumentSeries
                { instrumentSeriesId = assetSeriesId assetId
                , instrumentSeriesAssetId = assetId
                , instrumentSeriesKind = BaseSeries
                , instrumentSeriesIssuer = issuerId
                }
          )
        | (assetId, issuerId) <- Map.toList assetIssuers
        ]

buildPlayers :: Int -> Int -> ([Entity], Int)
buildPlayers count seed =
    let go (players, currentSeed) index =
            let (name, next) = drawName currentSeed index
                player = Entity (EntityId index) name PlayerEntity True
             in (players ++ [player], next)
     in foldl go ([], seed) [1 .. count]

drawName :: Int -> Int -> (String, Int)
drawName seed index =
    let namePool =
            [ "Vaska"
            , "Orin"
            , "Tessel"
            , "Moro"
            , "Enne"
            , "Ilya"
            , "Pavo"
            , "Suri"
            , "Deme"
            , "Rasal"
            ]
        (baseName, seed') = drawFromList namePool seed
     in (baseName ++ "-" ++ show index, seed')

buildInitialHoldings :: DefaultConfig -> EntityId -> [Entity] -> Int -> (Holdings, Int)
buildInitialHoldings config governmentId players seed =
    let governmentReserve = configGovernmentReserve config
        governmentHoldings =
            Map.fromList
                [ (TLN001, governmentReserve)
                , (TLN101, governmentReserve)
                , (TLN102, governmentReserve)
                , (TLN103, governmentReserve)
                ]
        baseHoldings = Map.singleton governmentId governmentHoldings
        go (holdings, currentSeed) player =
            let playerId = entityId player
                (bundle101, seed1) = drawBounded (configInitialBundleMax config + 1) currentSeed
                (bundle102, seed2) = drawBounded (configInitialBundleMax config + 1) seed1
                (bundle103, seed3) = drawBounded (configInitialBundleMax config + 1) seed2
                ledger =
                    Map.fromList
                        [ (TLN001, configStartingAccessTokens config)
                        , (TLN101, bundle101)
                        , (TLN102, bundle102)
                        , (TLN103, bundle103)
                        ]
                holdings' = Map.insert playerId ledger holdings
             in (holdings', seed3)
     in foldl go (baseHoldings, seed) players
