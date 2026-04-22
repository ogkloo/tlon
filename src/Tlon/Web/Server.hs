{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.Server
  ( runWebServer,
  )
where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import qualified Data.Text.Lazy as LazyText
import Data.Unique (hashUnique, newUnique)
import Lucid (renderText)
import Network.HTTP.Types.Status (status404)
import Paths_tlon (getDataFileName)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import Tlon.Core.State
import Tlon.Game.Default.Config
import Tlon.Web.Api
import Tlon.Web.State
import Tlon.Web.View
import Web.Scotty
import Web.Scotty.Internal.Types (ScottyException)

runWebServer :: Int -> IO ()
runWebServer port = do
  stateVar <- newTVarIO initialServerState
  reloadToken <- LazyText.pack . show . hashUnique <$> newUnique
  htmxPath <- resolveStaticAssetPath "htmx.min.js"
  agentationPath <- resolveStaticAssetPath "agentation.bundle.js"
  scotty port $ do
    get (regex "^/static/htmx\\.min\\.js$") $
      file htmxPath

    get (regex "^/static/agentation\\.bundle\\.js$") $
      file agentationPath

    get "/dev/reload-token" $
      text reloadToken

    get "/" $ do
      games <- liftIO (listGames <$> readTVarIO stateVar)
      html (renderText (renderIndexPage games))

    post "/games" $ do
      _ <- liftIO $
        atomically $
          stateTVar stateVar $ \serverState ->
            let (_, serverState') = createGame defaultConfig serverState
             in ((), serverState')
      redirect "/"

    get "/games/:gameId" $ do
      gameId <- GameId <$> pathParam "gameId"
      maybeGame <- liftIO (getGame gameId <$> readTVarIO stateVar)
      renderMaybeGame maybeGame

    post "/games/:gameId/resolve" $ do
      gameId <- GameId <$> pathParam "gameId"
      handleGameMutation stateVar gameId (resolveGame gameId)

    post "/games/:gameId/advance" $ do
      gameId <- GameId <$> pathParam "gameId"
      count <- formIntParamOr "count" 1
      handleGameMutation stateVar gameId (advanceGameBy count gameId)

    post "/games/:gameId/advance-to-round" $ do
      gameId <- GameId <$> pathParam "gameId"
      targetRound <- formIntParamOr "targetRound" 1
      handleGameMutation stateVar gameId (advanceGameToRound targetRound gameId)

    post "/games/:gameId/advance-to-end" $ do
      gameId <- GameId <$> pathParam "gameId"
      handleGameMutation stateVar gameId (advanceGameToEnd gameId)

    post "/games/:gameId/reset" $ do
      gameId <- GameId <$> pathParam "gameId"
      handleGameMutation stateVar gameId (resetGame gameId)

    get "/api/games/:gameId/state" $ do
      gameId <- GameId <$> pathParam "gameId"
      maybeGame <- liftIO (getGame gameId <$> readTVarIO stateVar)
      case maybeGame of
        Nothing -> do
          status status404
          json (objectError "game not found")
        Just runningGame ->
          json (gameJson runningGame)

    get "/api/games/:gameId/report" $ do
      gameId <- GameId <$> pathParam "gameId"
      maybeGame <- liftIO (getGame gameId <$> readTVarIO stateVar)
      case maybeGame of
        Nothing -> do
          status status404
          json (objectError "game not found")
        Just runningGame ->
          case gamePreviousReport (runningState runningGame) of
            Nothing -> json (objectError "no report yet")
            Just report -> json (reportJson report)

renderMaybeGame :: Maybe RunningGame -> ActionM ()
renderMaybeGame maybeGame =
  case maybeGame of
    Nothing -> do
      status status404
      html (renderText renderNotFoundPage)
    Just runningGame ->
      html (renderText (renderGamePage runningGame))

handleGameMutation :: TVar ServerState -> GameId -> (ServerState -> Maybe ServerState) -> ActionM ()
handleGameMutation stateVar gameId action = do
  updated <- liftIO $ atomically $ stateTVar stateVar (applyGameAction action)
  case updated of
    Just serverState' -> do
      htmxRequest <- isHtmxRequest
      case getGame gameId serverState' of
        Nothing -> do
          status status404
          html (renderText renderNotFoundPage)
        Just runningGame ->
          if htmxRequest
            then html (renderText (renderGameShell runningGame))
            else redirect (LazyText.pack ("/games/" ++ showGameId gameId))
    Nothing -> do
      status status404
      html (renderText renderNotFoundPage)

isHtmxRequest :: ActionM Bool
isHtmxRequest = do
  maybeHeader <- header "HX-Request"
  pure (maybeHeader == Just "true")

objectError :: LazyText.Text -> Value
objectError message =
  object ["error" .= message]

showGameId :: GameId -> String
showGameId (GameId gameId) = show gameId

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
