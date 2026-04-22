module Tlon.Core.OrderBook
  ( matchRound,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Tlon.Core.Types

data BookOrder = BookOrder
  { bookValidated :: ValidatedOrder,
    bookRemaining :: Quantity
  }

matchRound :: MatchingPolicy -> [ValidatedOrder] -> [Fill]
matchRound policy orders =
  case policy of
    PriceTimeFIFO -> matchPriceTimeFIFO orders
    UniformPriceBatch -> matchPriceTimeFIFO orders

matchPriceTimeFIFO :: [ValidatedOrder] -> [Fill]
matchPriceTimeFIFO orders =
  concatMap matchBook (Map.elems grouped)
  where
    grouped =
      Map.fromListWith
        (++)
        [(bookKey order, [order]) | order <- orders]

    bookKey validated =
      let order = validatedOrder validated
       in (orderMarketId order, orderBaseAsset order, orderQuoteAsset order)

matchBook :: [ValidatedOrder] -> [Fill]
matchBook orders =
  let buys = sortBuys [toBookOrder order | order <- orders, orderSide (validatedOrder order) == Buy]
      sells = sortSells [toBookOrder order | order <- orders, orderSide (validatedOrder order) == Sell]
   in go buys sells []

toBookOrder :: ValidatedOrder -> BookOrder
toBookOrder validated =
  BookOrder
    { bookValidated = validated,
      bookRemaining = orderQuantity (validatedOrder validated)
    }

sortBuys :: [BookOrder] -> [BookOrder]
sortBuys =
  List.sortBy
    (\left right ->
       compare (negate (orderLimitPrice (validatedOrder' left))) (negate (orderLimitPrice (validatedOrder' right)))
         <> compare (validatedSubmissionIndex (bookValidated left)) (validatedSubmissionIndex (bookValidated right))
         <> compare (orderId (validatedOrder' left)) (orderId (validatedOrder' right))
    )
  where
    validatedOrder' = validatedOrder . bookValidated

sortSells :: [BookOrder] -> [BookOrder]
sortSells =
  List.sortBy
    (\left right ->
       compare (orderLimitPrice (validatedOrder' left)) (orderLimitPrice (validatedOrder' right))
         <> compare (validatedSubmissionIndex (bookValidated left)) (validatedSubmissionIndex (bookValidated right))
         <> compare (orderId (validatedOrder' left)) (orderId (validatedOrder' right))
    )
  where
    validatedOrder' = validatedOrder . bookValidated

go :: [BookOrder] -> [BookOrder] -> [Fill] -> [Fill]
go buys sells acc =
  case (buys, sells) of
    (buy : remainingBuys, sell : remainingSells)
      | orderLimitPrice buyOrder < orderLimitPrice sellOrder -> reverse acc
      | otherwise ->
          let matchedQuantity = min (bookRemaining buy) (bookRemaining sell)
              fill =
                Fill
                  { fillBuyOrderId = orderId buyOrder,
                    fillSellOrderId = orderId sellOrder,
                    fillBuyer = orderEntityId buyOrder,
                    fillSeller = orderEntityId sellOrder,
                    fillMarketId = orderMarketId buyOrder,
                    fillBaseAsset = orderBaseAsset buyOrder,
                    fillQuoteAsset = orderQuoteAsset buyOrder,
                    fillQuantity = matchedQuantity,
                    fillPrice = orderLimitPrice sellOrder
                  }
              nextBuy =
                if bookRemaining buy == matchedQuantity
                  then remainingBuys
                  else buy {bookRemaining = bookRemaining buy - matchedQuantity} : remainingBuys
              nextSell =
                if bookRemaining sell == matchedQuantity
                  then remainingSells
                  else sell {bookRemaining = bookRemaining sell - matchedQuantity} : remainingSells
           in go nextBuy nextSell (fill : acc)
      where
        buyOrder = validatedOrder (bookValidated buy)
        sellOrder = validatedOrder (bookValidated sell)
    _ -> reverse acc
