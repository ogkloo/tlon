module Tlon.Core.Types (
    AssetId (..),
    Entity (..),
    EntityId (..),
    EntityKind (..),
    Fill (..),
    InstrumentSeries (..),
    InstrumentSeriesKind (..),
    InvalidReason (..),
    LotteryOffer (..),
    LotteryPurchase (..),
    Market (..),
    MarketRule (..),
    MarketId (..),
    MatchingPolicy (..),
    Order (..),
    OrderId (..),
    Price,
    Quantity,
    SeriesId,
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

ticketSeriesId :: Int -> Int -> AssetId -> Quantity -> Int -> Int -> Quantity -> SeriesId
ticketSeriesId roundIssued duration payoutAsset ticketPrice oddsNumerator oddsDenominator payoutQuantity =
    "LOTTO-"
        ++ show roundIssued
        ++ "-"
        ++ show duration
        ++ "-"
        ++ show payoutAsset
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

data InstrumentSeries = InstrumentSeries
    { instrumentSeriesId :: SeriesId
    , instrumentSeriesBaseAssetId :: Maybe AssetId
    , instrumentSeriesKind :: InstrumentSeriesKind
    , instrumentSeriesIssuer :: EntityId
    , instrumentSeriesRoundIssued :: Maybe Int
    , instrumentSeriesSettlementRound :: Maybe Int
    , instrumentSeriesTicketPrice :: Maybe Quantity
    , instrumentSeriesOddsNumerator :: Maybe Int
    , instrumentSeriesOddsDenominator :: Maybe Int
    , instrumentSeriesPayoutQuantity :: Maybe Quantity
    , instrumentSeriesPayoutAssetId :: Maybe AssetId
    }
    deriving (Eq, Show)

data LotteryOffer = LotteryOffer
    { lotteryOfferAssetId :: SeriesId
    , lotteryOfferTicketPrice :: Quantity
    , lotteryOfferOddsNumerator :: Int
    , lotteryOfferOddsDenominator :: Int
    , lotteryOfferPayoutQuantity :: Quantity
    , lotteryOfferDurationRounds :: Int
    }
    deriving (Eq, Show)

data LotteryPurchase = LotteryPurchase
    { lotteryPurchaseEntityId :: EntityId
    , lotteryPurchaseAssetId :: SeriesId
    , lotteryPurchaseQuantity :: Quantity
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
