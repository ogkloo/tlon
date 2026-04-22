module Tlon.Core.Types (
    AssetId (..),
    Entity (..),
    EntityId (..),
    EntityKind (..),
    Fill (..),
    InvalidReason (..),
    Market (..),
    MarketRule (..),
    MarketId (..),
    MatchingPolicy (..),
    Order (..),
    OrderId (..),
    Price,
    Quantity,
    Side (..),
    ValidatedOrder (..),
    allAbstractAssets,
    isCurrencyAsset,
    isAbstractAsset,
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

data AssetId
    = TLN001
    | TLN101
    | TLN102
    | TLN103
    deriving (Eq, Ord, Show, Enum, Bounded)

allAbstractAssets :: [AssetId]
allAbstractAssets = [TLN101, TLN102, TLN103]

isAbstractAsset :: AssetId -> Bool
isAbstractAsset asset = asset `elem` allAbstractAssets

isCurrencyAsset :: AssetId -> Bool
isCurrencyAsset asset =
    case asset of
        TLN001 -> True
        _ -> False

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
    , marketPairs :: [(AssetId, AssetId)]
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
    , orderBaseAsset :: AssetId
    , orderQuoteAsset :: AssetId
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
    , fillBaseAsset :: AssetId
    , fillQuoteAsset :: AssetId
    , fillQuantity :: Quantity
    , fillPrice :: Price
    }
    deriving (Eq, Show)
