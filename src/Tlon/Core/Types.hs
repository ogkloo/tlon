module Tlon.Core.Types (
    AssetId (..),
    Entity (..),
    EntityId (..),
    EntityKind (..),
    Fill (..),
    InstrumentOffering (..),
    InstrumentSeries (..),
    InstrumentSeriesKind (..),
    InstrumentTerms (..),
    InvalidReason (..),
    LotteryOfferingTerms (..),
    LotteryTerms (..),
    OfferingPurchase (..),
    OfferingTerms (..),
    Market (..),
    MarketRule (..),
    MarketId (..),
    MatchingPolicy (..),
    Order (..),
    OrderId (..),
    Price,
    Quantity,
    SeriesId,
    SeriesStatus (..),
    Side (..),
    ValidatedOrder (..),
    allAbstractAssets,
    assetSeriesId,
    isCurrencyAsset,
    isAbstractAsset,
    ticketSeriesId,
)
where

newtype EntityId = EntityId Int
    deriving (Eq, Ord, Show)

newtype MarketId = MarketId Int
    deriving (Eq, Ord, Show)

newtype OrderId = OrderId Int
    deriving (Eq, Ord, Show)

type Quantity = Int

type Price = Int

type SeriesId = String

data AssetId
    = TLN001
    | TLN101
    | TLN102
    | TLN103
    deriving (Eq, Ord, Show, Enum, Bounded)

allAbstractAssets :: [AssetId]
allAbstractAssets = [TLN101, TLN102, TLN103]

assetSeriesId :: AssetId -> SeriesId
assetSeriesId = show

ticketSeriesId :: Int -> Int -> SeriesId -> Quantity -> Int -> Int -> Quantity -> SeriesId
ticketSeriesId roundIssued duration payoutSeriesId ticketPrice oddsNumerator oddsDenominator payoutQuantity =
    "LOTTO-"
        ++ show roundIssued
        ++ "-"
        ++ show duration
        ++ "-"
        ++ payoutSeriesId
        ++ "-"
        ++ show ticketPrice
        ++ "-"
        ++ show oddsNumerator
        ++ "-"
        ++ show oddsDenominator
        ++ "-"
        ++ show payoutQuantity

isAbstractAsset :: AssetId -> Bool
isAbstractAsset asset = asset `elem` allAbstractAssets

isCurrencyAsset :: AssetId -> Bool
isCurrencyAsset asset =
    case asset of
        TLN001 -> True
        _ -> False

data InstrumentSeriesKind
    = BaseSeries
    | LotteryTicketSeries
    | RaffleTicketSeries
    | DerivativeSeries
    deriving (Eq, Show)

data SeriesStatus
    = BaseSeriesStatus
    | ActiveSeriesStatus
    | MaturedSeriesStatus
    | SettledSeriesStatus
    deriving (Eq, Show)

data InstrumentTerms
    = BaseInstrumentTerms AssetId
    | LotteryInstrumentTerms LotteryTerms
    | RaffleInstrumentTerms
    | DerivativeInstrumentTerms
    deriving (Eq, Show)

data LotteryTerms = LotteryTerms
    { lotteryTermsTicketPrice :: Quantity
    , lotteryTermsOddsNumerator :: Int
    , lotteryTermsOddsDenominator :: Int
    , lotteryTermsPayoutQuantity :: Quantity
    , lotteryTermsPayoutSeriesId :: SeriesId
    }
    deriving (Eq, Show)

data InstrumentSeries = InstrumentSeries
    { instrumentSeriesId :: SeriesId
    , instrumentSeriesKind :: InstrumentSeriesKind
    , instrumentSeriesIssuer :: EntityId
    , instrumentSeriesRoundIssued :: Maybe Int
    , instrumentSeriesSettlementRound :: Maybe Int
    , instrumentSeriesTerms :: InstrumentTerms
    }
    deriving (Eq, Show)

data OfferingTerms
    = LotteryOffering LotteryOfferingTerms
    deriving (Eq, Show)

data LotteryOfferingTerms = LotteryOfferingTerms
    { lotteryOfferingPayoutSeriesId :: SeriesId
    , lotteryOfferingTicketPrice :: Quantity
    , lotteryOfferingOddsNumerator :: Int
    , lotteryOfferingOddsDenominator :: Int
    , lotteryOfferingPayoutQuantity :: Quantity
    , lotteryOfferingDurationRounds :: Int
    }
    deriving (Eq, Show)

data InstrumentOffering = InstrumentOffering
    { instrumentOfferingSeriesId :: SeriesId
    , instrumentOfferingIssuer :: EntityId
    , instrumentOfferingTerms :: OfferingTerms
    }
    deriving (Eq, Show)

data OfferingPurchase = OfferingPurchase
    { offeringPurchaseEntityId :: EntityId
    , offeringPurchaseSeriesId :: SeriesId
    , offeringPurchaseQuantity :: Quantity
    }
    deriving (Eq, Show)

data EntityKind
    = GovernmentEntity
    | PlayerEntity
    deriving (Eq, Show)

data Entity = Entity
    { entityId :: EntityId
    , entityName :: String
    , entityKind :: EntityKind
    , entityAlive :: Bool
    }
    deriving (Eq, Show)

data MarketRule
    = QuoteAssetMustBeOwnerIssuedCurrency
    deriving (Eq, Show)

data Market = Market
    { marketId :: MarketId
    , marketName :: String
    , marketOwner :: EntityId
    , marketPairs :: [(SeriesId, SeriesId)]
    , marketRules :: [MarketRule]
    }
    deriving (Eq, Show)

data Side
    = Buy
    | Sell
    deriving (Eq, Show)

data Order = Order
    { orderId :: OrderId
    , orderEntityId :: EntityId
    , orderMarketId :: MarketId
    , orderSide :: Side
    , orderBaseAsset :: SeriesId
    , orderQuoteAsset :: SeriesId
    , orderQuantity :: Quantity
    , orderLimitPrice :: Price
    }
    deriving (Eq, Show)

data MatchingPolicy
    = PriceTimeFIFO
    | UniformPriceBatch
    deriving (Eq, Show)

data InvalidReason
    = InactiveEntity
    | NonPositiveQuantity
    | NonPositivePrice
    | UnsupportedPair
    | ViolatesMarketRule
    | InsufficientInventory
    | OpposingSidesSamePair
    deriving (Eq, Show)

data ValidatedOrder = ValidatedOrder
    { validatedOrder :: Order
    , validatedSubmissionIndex :: Int
    }
    deriving (Eq, Show)

data Fill = Fill
    { fillBuyOrderId :: OrderId
    , fillSellOrderId :: OrderId
    , fillBuyer :: EntityId
    , fillSeller :: EntityId
    , fillMarketId :: MarketId
    , fillBaseAsset :: SeriesId
    , fillQuoteAsset :: SeriesId
    , fillQuantity :: Quantity
    , fillPrice :: Price
    }
    deriving (Eq, Show)
