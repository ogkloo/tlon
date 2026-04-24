module Tlon.Core.Event (
    ExpiredOrder (..),
    GameEvent (..),
    Grant (..),
    InvalidOrder (..),
    LotteryIssuance (..),
    LotterySettlement (..),
    RoundReport (..),
    SurvivalResult (..),
)
where

import Tlon.Core.Types

data InvalidOrder = InvalidOrder
    { invalidSubmittedOrder :: Order
    , invalidReason :: InvalidReason
    }
    deriving (Eq, Show)

data ExpiredOrder = ExpiredOrder
    { expiredOrderId :: OrderId
    , expiredEntityId :: EntityId
    , expiredQuantity :: Quantity
    }
    deriving (Eq, Show)

data LotteryIssuance = LotteryIssuance
    { lotteryIssuanceEntityId :: EntityId
    , lotteryIssuanceSeriesId :: SeriesId
    , lotteryIssuanceTicketCount :: Quantity
    }
    deriving (Eq, Show)

data LotterySettlement = LotterySettlement
    { lotterySettlementEntityId :: EntityId
    , lotterySettlementSeriesId :: SeriesId
    , lotterySettlementTicketCount :: Quantity
    , lotterySettlementWinCount :: Quantity
    , lotterySettlementPayoutQuantity :: Quantity
    }
    deriving (Eq, Show)

data SurvivalResult = SurvivalResult
    { survivalEntityId :: EntityId
    , survivalPaidStake :: Bool
    , survivalRefunded :: Bool
    }
    deriving (Eq, Show)

data Grant = Grant
    { grantEntityId :: EntityId
    , grantAssetId :: AssetId
    , grantQuantity :: Quantity
    }
    deriving (Eq, Show)

data RoundReport = RoundReport
    { reportRoundNumber :: Int
    , reportActiveOfferings :: [InstrumentOffering]
    , reportSubmittedOrders :: [Order]
    , reportOfferingPurchases :: [OfferingPurchase]
    , reportInvalidOrders :: [InvalidOrder]
    , reportFills :: [Fill]
    , reportExpiredOrders :: [ExpiredOrder]
    , reportLotteryIssuances :: [LotteryIssuance]
    , reportLotterySettlements :: [LotterySettlement]
    , reportSurvivalResults :: [SurvivalResult]
    , reportRefundRecipient :: Maybe EntityId
    , reportNextRoundGrants :: [Grant]
    , reportNextActiveOfferings :: [InstrumentOffering]
    }
    deriving (Eq, Show)

data GameEvent
    = RoundResolved RoundReport
    deriving (Eq, Show)
