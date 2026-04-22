module Tlon.Core.Event
  ( ExpiredOrder (..),
    GameEvent (..),
    Grant (..),
    InvalidOrder (..),
    LotteryResult (..),
    RoundReport (..),
    SurvivalResult (..),
  )
where

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

data LotteryResult = LotteryResult
  { lotteryResultEntityId :: EntityId,
    lotteryResultAssetId :: AssetId,
    lotteryResultTicketCount :: Quantity,
    lotteryResultWinCount :: Quantity,
    lotteryResultPayoutQuantity :: Quantity
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
    reportLotteryMenu :: [LotteryOffer],
    reportSubmittedOrders :: [Order],
    reportLotteryPurchases :: [LotteryPurchase],
    reportInvalidOrders :: [InvalidOrder],
    reportFills :: [Fill],
    reportExpiredOrders :: [ExpiredOrder],
    reportLotteryResults :: [LotteryResult],
    reportSurvivalResults :: [SurvivalResult],
    reportRefundRecipient :: Maybe EntityId,
    reportNextRoundGrants :: [Grant],
    reportNextLotteryMenu :: [LotteryOffer]
  }
  deriving (Eq, Show)

data GameEvent
  = RoundResolved RoundReport
  deriving (Eq, Show)
