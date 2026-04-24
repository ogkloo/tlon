{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.Server (
    runWebServer,
)
where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import qualified Data.Text.Lazy as LazyText
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Unique (hashUnique, newUnique)
import Lucid (Html, renderText)
import Network.HTTP.Types.Status (status400, status404)
import Paths_tlon (getDataFileName)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Web.Api
import Tlon.Web.State
import Tlon.Web.View
import Web.Scotty
import Web.Scotty.Internal.Types (ScottyException)

runWebServer :: Int -> Bool -> IO ()
runWebServer port debugEnabled = do
    stateVar <- newTVarIO initialServerState
    reloadToken <- LazyText.pack . show . hashUnique <$> newUnique
    htmxPath <- resolveStaticAssetPath "htmx.min.js"
    maybeAgentationPath <-
        if debugEnabled
            then Just <$> resolveStaticAssetPath "agentation.bundle.js"
            else pure Nothing
    scotty port $ do
        get (regex "^/static/htmx\\.min\\.js$") $
            file htmxPath

        case maybeAgentationPath of
            Nothing -> pure ()
            Just agentationPath ->
                get (regex "^/static/agentation\\.bundle\\.js$") $
                    file agentationPath

        get "/dev/reload-token" $
            text reloadToken

        get "/" $ do
            games <- liftIO (listGames <$> readTVarIO stateVar)
            html (renderText (renderIndexPage debugEnabled games))

        when debugEnabled $
            post "/debug/reset-all" $ do
                liftIO $ atomically $ modifyTVar' stateVar resetAllGames
                redirect "/"

        post "/games" $ do
            playerName <- formStringParamOr "playerName" "Player"
            playerCount <- formIntParamOr "playerCount" 2
            npcCount <- formIntParamOr "npcCount" 0
            roundTimeLimitSeconds <- formOptionalIntParam "roundTimeLimitSeconds"
            (gameId, playerId) <- liftIO $
                atomically $
                    stateTVar stateVar $ \serverState ->
                        let (createdGameId, createdPlayerId, serverState') =
                                createLobby playerName playerCount npcCount roundTimeLimitSeconds serverState
                         in ((createdGameId, createdPlayerId), serverState')
            redirect (playerGamePath gameId playerId)

        get "/games/:gameId" $ do
            gameId <- GameId <$> pathParam "gameId"
            renderSyncedGame stateVar debugEnabled gameId Nothing False

        get "/games/:gameId/sync" $ do
            gameId <- GameId <$> pathParam "gameId"
            renderSyncedGame stateVar debugEnabled gameId Nothing True

        get "/games/:gameId/players/:playerId" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            renderSyncedGame stateVar debugEnabled gameId (Just playerId) False

        get "/games/:gameId/players/:playerId/sync" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            renderSyncedGame stateVar debugEnabled gameId (Just playerId) True

        post "/games/:gameId/join" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerName <- formStringParamOr "playerName" "Player"
            maybeJoined <- liftIO $
                atomically $
                    stateTVar stateVar $ \serverState ->
                        case joinGame gameId playerName serverState of
                            Nothing -> (Nothing, serverState)
                            Just (playerId, serverState') -> (Just playerId, serverState')
            case maybeJoined of
                Nothing -> redirect (gamePath gameId)
                Just playerId -> redirect (playerGamePath gameId playerId)

        post "/games/:gameId/start" $ do
            gameId <- GameId <$> pathParam "gameId"
            handleTimedGameMutation stateVar debugEnabled gameId Nothing (startGameAction gameId)

        post "/games/:gameId/players/:playerId/start" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            handleTimedGameMutation stateVar debugEnabled gameId (Just playerId) (startGameAction gameId)

        post "/games/:gameId/resolve" $ do
            gameId <- GameId <$> pathParam "gameId"
            handleTimedGameMutation stateVar debugEnabled gameId Nothing (resolveGameAction gameId)

        post "/games/:gameId/players/:playerId/submit" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            expectedRound <- formIntParamOr "expectedRound" 1
            handleTimedGameMutation stateVar debugEnabled gameId (Just playerId) (submitTurnAction expectedRound gameId playerId)

        post "/games/:gameId/players/:playerId/orders" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            sideText <- formStringParamOr "side" "Buy"
            seriesId <- formStringParamOr "seriesId" (assetSeriesId TLN101)
            quantity <- formIntParamOr "quantity" 1
            limitPrice <- formIntParamOr "limitPrice" 1
            case parseSide sideText of
                Just side ->
                    handlePureGameMutation stateVar debugEnabled gameId (Just playerId) (stageLimitOrder gameId playerId side seriesId quantity limitPrice)
                Nothing -> renderBadRequest stateVar debugEnabled gameId (Just playerId)

        post "/games/:gameId/players/:playerId/orders/:orderId/delete" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            orderIdValue <- pathParam "orderId"
            handlePureGameMutation stateVar debugEnabled gameId (Just playerId) (removeStagedOrder gameId playerId (OrderId orderIdValue))

        post "/games/:gameId/players/:playerId/offerings" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            seriesId <- formStringParamOr "seriesId" ""
            quantity <- formIntParamOr "quantity" 0
            handlePureGameMutation stateVar debugEnabled gameId (Just playerId) (setOfferingPurchaseCount gameId playerId seriesId quantity)

        post "/games/:gameId/markets/:marketId/rules" $ do
            gameId <- GameId <$> pathParam "gameId"
            marketId <- MarketId <$> pathParam "marketId"
            enabledText <- formStringParamOr "enabled" "false"
            case parseMarketRule "QuoteAssetMustBeOwnerIssuedCurrency" of
                Just marketRule ->
                    handlePureGameMutation stateVar debugEnabled gameId Nothing (setMarketRuleEnabled gameId Nothing marketId marketRule (parseEnabled enabledText))
                Nothing -> renderBadRequest stateVar debugEnabled gameId Nothing

        post "/games/:gameId/players/:playerId/markets/:marketId/rules" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            marketId <- MarketId <$> pathParam "marketId"
            enabledText <- formStringParamOr "enabled" "false"
            case parseMarketRule "QuoteAssetMustBeOwnerIssuedCurrency" of
                Just marketRule ->
                    handlePureGameMutation stateVar debugEnabled gameId (Just playerId) (setMarketRuleEnabled gameId (Just playerId) marketId marketRule (parseEnabled enabledText))
                Nothing -> renderBadRequest stateVar debugEnabled gameId (Just playerId)

        post "/games/:gameId/advance" $ do
            gameId <- GameId <$> pathParam "gameId"
            count <- formIntParamOr "count" 1
            handlePureGameMutation stateVar debugEnabled gameId Nothing (advanceGameBy count gameId)

        post "/games/:gameId/advance-to-round" $ do
            gameId <- GameId <$> pathParam "gameId"
            targetRound <- formIntParamOr "targetRound" 1
            handlePureGameMutation stateVar debugEnabled gameId Nothing (advanceGameToRound targetRound gameId)

        post "/games/:gameId/advance-to-end" $ do
            gameId <- GameId <$> pathParam "gameId"
            handlePureGameMutation stateVar debugEnabled gameId Nothing (advanceGameToEnd gameId)

        post "/games/:gameId/reset" $ do
            gameId <- GameId <$> pathParam "gameId"
            handleTimedGameMutation stateVar debugEnabled gameId Nothing (resetGameAction gameId)

        post "/games/:gameId/players/:playerId/reset" $ do
            gameId <- GameId <$> pathParam "gameId"
            playerId <- PlayerId <$> pathParam "playerId"
            handleTimedGameMutation stateVar debugEnabled gameId (Just playerId) (resetGameAction gameId)

        get "/api/games/:gameId/state" $ do
            gameId <- GameId <$> pathParam "gameId"
            now <- liftIO getCurrentTime
            maybeGame <- liftIO (lookupSyncedGame stateVar now gameId)
            case maybeGame of
                Nothing -> do
                    status status404
                    json (objectError "game not found")
                Just runningGame ->
                    json (gameJson runningGame)

        get "/api/games/:gameId/report" $ do
            gameId <- GameId <$> pathParam "gameId"
            now <- liftIO getCurrentTime
            maybeGame <- liftIO (lookupSyncedGame stateVar now gameId)
            case maybeGame of
                Nothing -> do
                    status status404
                    json (objectError "game not found")
                Just runningGame ->
                    case gamePreviousReport (runningState runningGame) of
                        Nothing -> json (objectError "no report yet")
                        Just report -> json (reportJson report)

renderSyncedGame :: TVar ServerState -> Bool -> GameId -> Maybe PlayerId -> Bool -> ActionM ()
renderSyncedGame stateVar debugEnabled gameId maybePlayerId renderShellOnly = do
    now <- liftIO getCurrentTime
    maybeGame <- liftIO (lookupSyncedGame stateVar now gameId)
    case maybeGame of
        Nothing -> do
            status status404
            html (renderText renderNotFoundPage)
        Just runningGame ->
            case resolveCurrentPlayer runningGame maybePlayerId of
                Nothing -> do
                    status status404
                    html (renderText renderNotFoundPage)
                Just maybePlayer -> do
                    let maybeSecondsRemaining = roundSecondsRemaining now runningGame
                    if renderShellOnly
                        then html (renderText (renderShellForGame debugEnabled runningGame maybePlayer maybeSecondsRemaining))
                        else html (renderText (renderGamePage debugEnabled runningGame maybePlayer maybeSecondsRemaining))

renderShellForGame :: Bool -> RunningGame -> Maybe HumanPlayer -> Maybe Int -> Html ()
renderShellForGame debugEnabled runningGame maybePlayer maybeSecondsRemaining =
    if runningStarted runningGame
        then renderGameShell debugEnabled runningGame maybePlayer maybeSecondsRemaining
        else renderLobbyShell debugEnabled runningGame maybePlayer

resolveCurrentPlayer :: RunningGame -> Maybe PlayerId -> Maybe (Maybe HumanPlayer)
resolveCurrentPlayer runningGame maybePlayerId =
    case maybePlayerId of
        Nothing -> Just Nothing
        Just playerId -> Just <$> getPlayer playerId runningGame

handleTimedGameMutation ::
    TVar ServerState ->
    Bool ->
    GameId ->
    Maybe PlayerId ->
    (UTCTime -> ServerState -> Maybe ServerState) ->
    ActionM ()
handleTimedGameMutation stateVar debugEnabled gameId maybePlayerId action = do
    now <- liftIO getCurrentTime
    updated <- liftIO $ atomically $ stateTVar stateVar (applyGameAction (action now))
    renderMutationResult now stateVar debugEnabled gameId maybePlayerId updated

handlePureGameMutation ::
    TVar ServerState ->
    Bool ->
    GameId ->
    Maybe PlayerId ->
    (ServerState -> Maybe ServerState) ->
    ActionM ()
handlePureGameMutation stateVar debugEnabled gameId maybePlayerId action = do
    now <- liftIO getCurrentTime
    updated <- liftIO $ atomically $ stateTVar stateVar (applyGameAction action)
    renderMutationResult now stateVar debugEnabled gameId maybePlayerId updated

renderMutationResult :: UTCTime -> TVar ServerState -> Bool -> GameId -> Maybe PlayerId -> Maybe ServerState -> ActionM ()
renderMutationResult now stateVar debugEnabled gameId maybePlayerId updated =
    case updated of
        Nothing -> do
            status status400
            html (renderText renderNotFoundPage)
        Just _ -> do
            htmxRequest <- isHtmxRequest
            maybeGame <- liftIO (lookupSyncedGame stateVar now gameId)
            case maybeGame of
                Nothing -> do
                    status status404
                    html (renderText renderNotFoundPage)
                Just runningGame ->
                    case resolveCurrentPlayer runningGame maybePlayerId of
                        Nothing -> do
                            status status404
                            html (renderText renderNotFoundPage)
                        Just maybePlayer -> do
                            let maybeSecondsRemaining = roundSecondsRemaining now runningGame
                            if htmxRequest
                                then html (renderText (renderGameShell debugEnabled runningGame maybePlayer maybeSecondsRemaining))
                                else redirect (scopedGamePath gameId maybePlayerId)

renderBadRequest :: TVar ServerState -> Bool -> GameId -> Maybe PlayerId -> ActionM ()
renderBadRequest stateVar debugEnabled gameId maybePlayerId = do
    status status400
    renderSyncedGame stateVar debugEnabled gameId maybePlayerId True

lookupSyncedGame :: TVar ServerState -> UTCTime -> GameId -> IO (Maybe RunningGame)
lookupSyncedGame stateVar now gameId =
    atomically $
        stateTVar stateVar $ \serverState ->
            let syncedState = syncServerState now gameId serverState
             in (getGame gameId syncedState, syncedState)

syncServerState :: UTCTime -> GameId -> ServerState -> ServerState
syncServerState now gameId serverState =
    case syncGame now gameId serverState of
        Nothing -> serverState
        Just serverState' -> serverState'

type TimedMutation = UTCTime -> ServerState -> Maybe ServerState

startGameAction :: GameId -> TimedMutation
startGameAction gameId now = startGame now gameId

resolveGameAction :: GameId -> TimedMutation
resolveGameAction gameId now = resolveGame now gameId

resetGameAction :: GameId -> TimedMutation
resetGameAction gameId now = resetGame now gameId

submitTurnAction :: Int -> GameId -> PlayerId -> TimedMutation
submitTurnAction expectedRound gameId playerId now =
    submitTurn now expectedRound gameId playerId

isHtmxRequest :: ActionM Bool
isHtmxRequest = do
    maybeHeader <- header "HX-Request"
    pure (maybeHeader == Just "true")

objectError :: LazyText.Text -> Value
objectError message =
    object ["error" .= message]

showGameId :: GameId -> String
showGameId (GameId gameId) = show gameId

showPlayerId :: PlayerId -> String
showPlayerId (PlayerId playerId) = show playerId

gamePath :: GameId -> LazyText.Text
gamePath gameId =
    LazyText.pack ("/games/" ++ showGameId gameId)

playerGamePath :: GameId -> PlayerId -> LazyText.Text
playerGamePath gameId playerId =
    LazyText.pack ("/games/" ++ showGameId gameId ++ "/players/" ++ showPlayerId playerId)

scopedGamePath :: GameId -> Maybe PlayerId -> LazyText.Text
scopedGamePath gameId maybePlayerId =
    case maybePlayerId of
        Nothing -> gamePath gameId
        Just playerId -> playerGamePath gameId playerId

findRepoAssetPath :: FilePath -> IO FilePath
findRepoAssetPath assetName = do
    executablePath <- getExecutablePath
    let startingDir = takeDirectory executablePath
        candidateDirs = take 12 (iterate takeDirectory startingDir)
        candidatePaths = [dir </> "static" </> assetName | dir <- candidateDirs]
    findExistingPath ("static" </> assetName) candidatePaths

findExistingPath :: FilePath -> [FilePath] -> IO FilePath
findExistingPath fallbackPath paths =
    case paths of
        [] -> pure fallbackPath
        path : remainingPaths -> do
            exists <- doesFileExist path
            if exists
                then pure path
                else findExistingPath fallbackPath remainingPaths

resolveStaticAssetPath :: FilePath -> IO FilePath
resolveStaticAssetPath assetName = do
    installedPath <- getDataFileName ("static/" ++ assetName)
    hasInstalledAsset <- doesFileExist installedPath
    if hasInstalledAsset
        then pure installedPath
        else findRepoAssetPath assetName

applyGameAction :: (ServerState -> Maybe ServerState) -> ServerState -> (Maybe ServerState, ServerState)
applyGameAction action serverState =
    let maybeState = action serverState
     in (maybeState, maybe serverState id maybeState)

formIntParamOr :: LazyText.Text -> Int -> ActionM Int
formIntParamOr name fallbackValue =
    formParam name `rescue` handleScottyException
  where
    handleScottyException :: ScottyException -> ActionM Int
    handleScottyException _ = pure fallbackValue

formOptionalIntParam :: LazyText.Text -> ActionM (Maybe Int)
formOptionalIntParam name =
    (Just <$> formParam name) `rescue` handleScottyException
  where
    handleScottyException :: ScottyException -> ActionM (Maybe Int)
    handleScottyException _ = pure Nothing

formStringParamOr :: LazyText.Text -> String -> ActionM String
formStringParamOr name fallbackValue =
    formParam name `rescue` handleScottyException
  where
    handleScottyException :: ScottyException -> ActionM String
    handleScottyException _ = pure fallbackValue

parseSide :: String -> Maybe Side
parseSide rawValue =
    case rawValue of
        "Buy" -> Just Buy
        "Sell" -> Just Sell
        _ -> Nothing

parseAssetId :: String -> Maybe AssetId
parseAssetId rawValue =
    case rawValue of
        "TLN101" -> Just TLN101
        "TLN102" -> Just TLN102
        "TLN103" -> Just TLN103
        _ -> Nothing

parseMarketRule :: String -> Maybe MarketRule
parseMarketRule rawValue =
    case rawValue of
        "QuoteAssetMustBeOwnerIssuedCurrency" -> Just QuoteAssetMustBeOwnerIssuedCurrency
        _ -> Nothing

parseEnabled :: String -> Bool
parseEnabled rawValue =
    rawValue == "true"
