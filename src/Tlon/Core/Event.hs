module Tlon.Core.Event
  ( ExpiredOrder (..),
    GameEvent (..),
    Grant (..),
    InvalidOrder (..),
    Redemption (..),
    RoundReport (..),
    SurvivalResult (..),
  )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Tlon.Core.Types

data InvalidOrder = InvalidOrder
  { invalidSubmittedOrder :: Order,
    invalidReason :: InvalidReason
  }
  deriving (Eq, Show)

data ExpiredOrder = ExpiredOrder
  { expiredOrderId :: OrderId,
    expiredEntityId :: EntityId,
    expiredQuantity :: Quantity
  }
  deriving (Eq, Show)

data Redemption = Redemption
  { redemptionEntityId :: EntityId,
    redemptionAssetId :: AssetId,
    redemptionQuantity :: Quantity,
    redemptionPayout :: Quantity
  }
  deriving (Eq, Show)

data SurvivalResult = SurvivalResult
  { survivalEntityId :: EntityId,
    survivalPaidStake :: Bool,
    survivalRefunded :: Bool
  }
  deriving (Eq, Show)

data Grant = Grant
  { grantEntityId :: EntityId,
    grantAssetId :: AssetId,
    grantQuantity :: Quantity
  }
  deriving (Eq, Show)

data RoundReport = RoundReport
  { reportRoundNumber :: Int,
    reportRedemptionTable :: Map AssetId Quantity,
    reportSubmittedOrders :: [Order],
    reportInvalidOrders :: [InvalidOrder],
    reportFills :: [Fill],
    reportExpiredOrders :: [ExpiredOrder],
    reportRedemptions :: [Redemption],
    reportSurvivalResults :: [SurvivalResult],
    reportRefundRecipient :: Maybe EntityId,
    reportNextRoundGrants :: [Grant],
    reportNextRedemptionTable :: Map AssetId Quantity
  }
  deriving (Eq, Show)

data GameEvent
  = RoundResolved RoundReport
  deriving (Eq, Show)
