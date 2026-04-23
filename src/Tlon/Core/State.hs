module Tlon.Core.State (
    GameState (..),
    Holdings,
    PositionLedger,
    RoundInputs (..),
    SeriesCatalog,
    adjustBalance,
    assetIdForSeries,
    balanceOf,
    emptyLedger,
    findGovernmentId,
    livingMortals,
    seriesForAsset,
    setEntityAlive,
)
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Tlon.Core.Event
import Tlon.Core.Types

type PositionLedger = Map SeriesId Quantity

type Holdings = Map EntityId PositionLedger

type SeriesCatalog = Map SeriesId InstrumentSeries

data GameState = GameState
    { gameRoundNumber :: Int
    , gameMatchingPolicy :: MatchingPolicy
    , gameSeed :: Int
    , gameEntities :: Map EntityId Entity
    , gameAssetIssuers :: Map AssetId EntityId
    , gameSeriesCatalog :: SeriesCatalog
    , gameMarkets :: Map MarketId Market
    , gameHoldings :: Holdings
    , gameLotteryMenu :: [LotteryOffer]
    , gamePreviousReport :: Maybe RoundReport
    , gameWinner :: Maybe EntityId
    }
    deriving (Eq, Show)

data RoundInputs = RoundInputs
    { roundOrders :: [Order]
    , roundLotteryPurchases :: [LotteryPurchase]
    }
    deriving (Eq, Show)

emptyLedger :: PositionLedger
emptyLedger = Map.empty

balanceOf :: Holdings -> EntityId -> SeriesId -> Quantity
balanceOf holdings entity seriesId =
    case Map.lookup entity holdings of
        Nothing -> 0
        Just ledger -> Map.findWithDefault 0 seriesId ledger

adjustBalance :: EntityId -> SeriesId -> Quantity -> Holdings -> Holdings
adjustBalance entity seriesId delta holdings =
    let oldLedger = Map.findWithDefault emptyLedger entity holdings
        newBalance = Map.findWithDefault 0 seriesId oldLedger + delta
        newLedger
            | newBalance == 0 = Map.delete seriesId oldLedger
            | otherwise = Map.insert seriesId newBalance oldLedger
     in if Map.null newLedger
            then Map.delete entity holdings
            else Map.insert entity newLedger holdings

seriesForAsset :: GameState -> AssetId -> Maybe InstrumentSeries
seriesForAsset state assetId =
    Map.lookup (assetSeriesId assetId) (gameSeriesCatalog state)

assetIdForSeries :: GameState -> SeriesId -> Maybe AssetId
assetIdForSeries state seriesId =
    instrumentSeriesBaseAssetId =<< Map.lookup seriesId (gameSeriesCatalog state)

setEntityAlive :: EntityId -> Bool -> Map EntityId Entity -> Map EntityId Entity
setEntityAlive entityId' aliveFlag =
    Map.adjust (\entity -> entity{entityAlive = aliveFlag}) entityId'

livingMortals :: GameState -> [EntityId]
livingMortals state =
    [ entityId entity
    | entity <- Map.elems (gameEntities state)
    , entityKind entity == PlayerEntity
    , entityAlive entity
    ]

findGovernmentId :: GameState -> EntityId
findGovernmentId state =
    case [entityId entity | entity <- Map.elems (gameEntities state), entityKind entity == GovernmentEntity] of
        governmentId : _ -> governmentId
        [] -> error "GameState is missing a government entity."
