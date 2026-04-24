module Tlon.Game.Default.View (
    renderGameState,
)
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Tlon.Core.State
import Tlon.Core.Types

renderGameState :: GameState -> String
renderGameState state =
    unlines $
        [ "Tlon default game"
        , "Round: " ++ show (gameRoundNumber state)
        , "Winner: " ++ maybe "none" showEntityId (gameWinner state)
        , "Active offerings: " ++ renderOfferings (gameActiveOfferings state)
        , "Series catalog:"
        ]
            ++ map (renderSeriesLine state) (Map.elems (gameSeriesCatalog state))
            ++ ["Holdings:"]
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

renderSeriesLine :: GameState -> InstrumentSeries -> String
renderSeriesLine state series =
    "  - "
        ++ instrumentSeriesId series
        ++ " ["
        ++ show (instrumentSeriesKind series)
        ++ ", "
        ++ show (seriesStatus state series)
        ++ ", outstanding="
        ++ show (seriesOutstandingQuantity state (instrumentSeriesId series))
        ++ "]"

renderOfferings :: [InstrumentOffering] -> String
renderOfferings offerings =
    if null offerings
        then "[]"
        else
            "["
                ++ List.intercalate ", " (map renderOffering offerings)
                ++ "]"

renderOffering :: InstrumentOffering -> String
renderOffering offering =
    case instrumentOfferingTerms offering of
        LotteryOffering terms ->
            instrumentOfferingSeriesId offering
                ++ " @ "
                ++ show (lotteryOfferingTicketPrice terms)
                ++ " for "
                ++ show (lotteryOfferingOddsNumerator terms)
                ++ "/"
                ++ show (lotteryOfferingOddsDenominator terms)
                ++ " paying "
                ++ show (lotteryOfferingPayoutQuantity terms)
                ++ " "
                ++ lotteryOfferingPayoutSeriesId terms

renderLedger :: Map.Map SeriesId Quantity -> String
renderLedger ledger =
    if Map.null ledger
        then "{}"
        else
            "{"
                ++ List.intercalate ", " [show asset ++ "=" ++ show quantity | (asset, quantity) <- Map.toList ledger]
                ++ "}"
