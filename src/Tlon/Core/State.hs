module Tlon.Core.State (
    AssetLedger,
    GameState (..),
    Holdings,
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

type AssetLedger = Map AssetId Quantity

type Holdings = Map EntityId AssetLedger

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

emptyLedger :: AssetLedger
emptyLedger = Map.empty

balanceOf :: Holdings -> EntityId -> AssetId -> Quantity
balanceOf holdings entity asset =
    case Map.lookup entity holdings of
        Nothing -> 0
        Just ledger -> Map.findWithDefault 0 asset ledger

adjustBalance :: EntityId -> AssetId -> Quantity -> Holdings -> Holdings
adjustBalance entity asset delta holdings =
    let oldLedger = Map.findWithDefault emptyLedger entity holdings
        newBalance = Map.findWithDefault 0 asset oldLedger + delta
        newLedger
            | newBalance == 0 = Map.delete asset oldLedger
            | otherwise = Map.insert asset newBalance oldLedger
     in if Map.null newLedger
            then Map.delete entity holdings
            else Map.insert entity newLedger holdings

seriesForAsset :: GameState -> AssetId -> Maybe InstrumentSeries
seriesForAsset state assetId =
    Map.lookup (assetSeriesId assetId) (gameSeriesCatalog state)

assetIdForSeries :: GameState -> SeriesId -> Maybe AssetId
assetIdForSeries state seriesId =
    instrumentSeriesAssetId <$> Map.lookup seriesId (gameSeriesCatalog state)

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
