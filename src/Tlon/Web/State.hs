{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.State
  ( GameId (..),
    RunningGame (..),
    ServerState (..),
    advanceGameBy,
    advanceGameToEnd,
    advanceGameToRound,
    createGame,
    getGame,
    initialServerState,
    listGames,
    resetGame,
    resolveGame,
  )
where

import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Tlon.Core.Engine
import Tlon.Core.Event
import Tlon.Core.State
import Tlon.Game.Default.Config
import Tlon.Game.Default.Setup

newtype GameId = GameId Int
  deriving (Eq, Ord, Show)

data RunningGame = RunningGame
  { runningGameId :: GameId,
    runningConfig :: DefaultConfig,
    runningState :: GameState,
    runningHistory :: [RoundReport]
  }
  deriving (Eq, Show)

data ServerState = ServerState
  { serverNextGameId :: Int,
    serverGames :: Map GameId RunningGame
  }
  deriving (Eq, Show)

initialServerState :: ServerState
initialServerState =
  snd (createGame defaultConfig emptyServerState)

emptyServerState :: ServerState
emptyServerState =
  ServerState
    { serverNextGameId = 1,
      serverGames = Map.empty
    }

createGame :: DefaultConfig -> ServerState -> (GameId, ServerState)
createGame config serverState =
  let gameId = GameId (serverNextGameId serverState)
      runningGame =
        RunningGame
          { runningGameId = gameId,
            runningConfig = config,
            runningState = initialState config,
            runningHistory = []
          }
      games' = Map.insert gameId runningGame (serverGames serverState)
   in
    ( gameId,
      serverState
        { serverNextGameId = serverNextGameId serverState + 1,
          serverGames = games'
        }
    )

listGames :: ServerState -> [RunningGame]
listGames =
  List.sortOn runningGameId . Map.elems . serverGames

getGame :: GameId -> ServerState -> Maybe RunningGame
getGame gameId = Map.lookup gameId . serverGames

resolveGame :: GameId -> ServerState -> Maybe ServerState
resolveGame gameId serverState = do
  runningGame <- getGame gameId serverState
  let updatedGame = stepRunningGame runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

resetGame :: GameId -> ServerState -> Maybe ServerState
resetGame gameId serverState = do
  runningGame <- getGame gameId serverState
  let resetGameState =
        runningGame
          { runningState = initialState (runningConfig runningGame),
            runningHistory = []
          }
  pure serverState {serverGames = Map.insert gameId resetGameState (serverGames serverState)}

advanceGameBy :: Int -> GameId -> ServerState -> Maybe ServerState
advanceGameBy count gameId serverState = do
  runningGame <- getGame gameId serverState
  let updatedGame = iterateGameSteps (max 0 count) runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

advanceGameToRound :: Int -> GameId -> ServerState -> Maybe ServerState
advanceGameToRound targetRound gameId serverState = do
  runningGame <- getGame gameId serverState
  let updatedGame = iterateUntilTargetRound (max 1 targetRound) runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

advanceGameToEnd :: GameId -> ServerState -> Maybe ServerState
advanceGameToEnd gameId serverState = do
  runningGame <- getGame gameId serverState
  let updatedGame = iterateUntilWinner maxAdvanceSteps runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

maxAdvanceSteps :: Int
maxAdvanceSteps = 10000

stepRunningGame :: RunningGame -> RunningGame
stepRunningGame runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise =
      let (state', events) = stepRound (runningConfig runningGame) (RoundInputs []) (runningState runningGame)
          reports = [report | RoundResolved report <- events]
       in
        runningGame
          { runningState = state',
            runningHistory = runningHistory runningGame ++ reports
          }

iterateGameSteps :: Int -> RunningGame -> RunningGame
iterateGameSteps stepCount runningGame
  | stepCount <= 0 = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise = iterateGameSteps (stepCount - 1) (stepRunningGame runningGame)

iterateUntilTargetRound :: Int -> RunningGame -> RunningGame
iterateUntilTargetRound targetRound runningGame
  | gameRoundNumber (runningState runningGame) >= targetRound = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise = iterateUntilTargetRound targetRound (stepRunningGame runningGame)

iterateUntilWinner :: Int -> RunningGame -> RunningGame
iterateUntilWinner remainingSteps runningGame
  | remainingSteps <= 0 = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise = iterateUntilWinner (remainingSteps - 1) (stepRunningGame runningGame)
