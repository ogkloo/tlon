module Tlon.Core.State
  ( AssetLedger,
    GameState (..),
    Holdings,
    RoundInputs (..),
    adjustBalance,
    balanceOf,
    emptyLedger,
    findGovernmentId,
    livingMortals,
    setEntityAlive,
  )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Tlon.Core.Event
import Tlon.Core.Types

type AssetLedger = Map AssetId Quantity

type Holdings = Map EntityId AssetLedger

data GameState = GameState
  { gameRoundNumber :: Int,
    gameMatchingPolicy :: MatchingPolicy,
    gameSeed :: Int,
    gameEntities :: Map EntityId Entity,
    gameMarkets :: Map MarketId Market,
    gameHoldings :: Holdings,
    gameRedemptionTable :: Map AssetId Quantity,
    gamePreviousReport :: Maybe RoundReport,
    gameWinner :: Maybe EntityId
  }
  deriving (Eq, Show)

data RoundInputs = RoundInputs
  { roundOrders :: [Order]
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

setEntityAlive :: EntityId -> Bool -> Map EntityId Entity -> Map EntityId Entity
setEntityAlive entityId' aliveFlag =
  Map.adjust (\entity -> entity {entityAlive = aliveFlag}) entityId'

livingMortals :: GameState -> [EntityId]
livingMortals state =
  [ entityId entity
    | entity <- Map.elems (gameEntities state),
      entityKind entity == PlayerEntity,
      entityAlive entity
  ]

findGovernmentId :: GameState -> EntityId
findGovernmentId state =
  case [entityId entity | entity <- Map.elems (gameEntities state), entityKind entity == GovernmentEntity] of
    governmentId : _ -> governmentId
    [] -> error "GameState is missing a government entity."
