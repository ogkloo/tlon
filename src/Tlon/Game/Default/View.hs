module Tlon.Game.Default.View
  ( renderGameState,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Tlon.Core.State
import Tlon.Core.Types

renderGameState :: GameState -> String
renderGameState state =
  unlines $
    [ "Tlon default game",
      "Round: " ++ show (gameRoundNumber state),
      "Winner: " ++ maybe "none" showEntityId (gameWinner state),
      "Lottery menu: " ++ renderLotteryMenu (gameLotteryMenu state),
      "Holdings:"
    ]
      ++ map renderEntityLine (Map.elems (gameEntities state))
  where
    renderEntityLine entity =
      let ledger = Map.findWithDefault Map.empty (entityId entity) (gameHoldings state)
       in "  - " ++ entityName entity ++ statusSuffix entity ++ ": " ++ renderLedger ledger
    statusSuffix entity =
      if entityAlive entity
        then ""
        else " [eliminated]"

showEntityId :: EntityId -> String
showEntityId = show

renderLotteryMenu :: [LotteryOffer] -> String
renderLotteryMenu offers =
  if null offers
    then "[]"
    else
      "["
        ++ List.intercalate ", " (map renderLotteryOffer offers)
        ++ "]"

renderLotteryOffer :: LotteryOffer -> String
renderLotteryOffer offer =
  show (lotteryOfferAssetId offer)
    ++ " @ "
    ++ show (lotteryOfferTicketPrice offer)
    ++ " for "
    ++ show (lotteryOfferOddsNumerator offer)
    ++ "/"
    ++ show (lotteryOfferOddsDenominator offer)
    ++ " paying "
    ++ show (lotteryOfferPayoutQuantity offer)

renderLedger :: Map.Map SeriesId Quantity -> String
renderLedger ledger =
  if Map.null ledger
    then "{}"
    else
      "{"
        ++ List.intercalate ", " [show asset ++ "=" ++ show quantity | (asset, quantity) <- Map.toList ledger]
        ++ "}"
