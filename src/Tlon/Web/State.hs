{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.State
  ( GameId (..),
    HumanPlayer (..),
    PlayerId (..),
    PlayerRoundPlan (..),
    RunningGame (..),
    ServerState (..),
    TicketLotteryResult (..),
    advanceGameBy,
    advanceGameToEnd,
    advanceGameToRound,
    createGame,
    createLobby,
    getGame,
    getPlayer,
    getPlayerPlan,
    humanSeatCount,
    initialServerState,
    joinGame,
    listGames,
    playerCanEditPlan,
    playerCanSubmitTurn,
    playerSubmittedTurn,
    removeStagedOrder,
    requiredSubmitters,
    resetGame,
    resetAllGames,
    resolveGame,
    roundSecondsRemaining,
    setTicketCount,
    stageLimitOrder,
    startGame,
    submitTurn,
    syncGame,
  )
where

import qualified Data.List as List
import Data.List (dropWhileEnd, find, foldl', nub)
import Data.Maybe (isJust, maybeToList)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Tlon.Core.Engine
import Tlon.Core.Event
import Tlon.Core.Rng
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Game.Default.Config
import Tlon.Game.Default.Setup

newtype GameId = GameId Int
  deriving (Eq, Ord, Show)

newtype PlayerId = PlayerId Int
  deriving (Eq, Ord, Show)

data HumanPlayer = HumanPlayer
  { humanPlayerId :: PlayerId,
    humanPlayerName :: String,
    humanPlayerEntityId :: Maybe EntityId
  }
  deriving (Eq, Show)

data PlayerRoundPlan = PlayerRoundPlan
  { planOrders :: [Order],
    planTicketCount :: Int
  }
  deriving (Eq, Show)

data TicketLotteryResult = TicketLotteryResult
  { ticketLotteryRoundNumber :: Int,
    ticketLotteryPurchases :: [(EntityId, Int)],
    ticketLotteryWinner :: Maybe EntityId
  }
  deriving (Eq, Show)

data RunningGame = RunningGame
  { runningGameId :: GameId,
    runningConfig :: DefaultConfig,
    runningSeatCount :: Int,
    runningHumanSeatCount :: Int,
    runningNpcCount :: Int,
    runningParticipants :: [HumanPlayer],
    runningStarted :: Bool,
    runningRoundTimeLimitSeconds :: Maybe Int,
    runningRoundDeadline :: Maybe UTCTime,
    runningSubmittedPlayers :: [PlayerId],
    runningRoundPlans :: Map PlayerId PlayerRoundPlan,
    runningNextOrderId :: Int,
    runningLatestTicketLottery :: Maybe TicketLotteryResult,
    runningTicketHistory :: [TicketLotteryResult],
    runningState :: GameState,
    runningHistory :: [RoundReport]
  }
  deriving (Eq, Show)

data ServerState = ServerState
  { serverNextGameId :: Int,
    serverNextPlayerId :: Int,
    serverGames :: Map GameId RunningGame
  }
  deriving (Eq, Show)

initialServerState :: ServerState
initialServerState = emptyServerState

emptyServerState :: ServerState
emptyServerState =
  ServerState
    { serverNextGameId = 1,
      serverNextPlayerId = 1,
      serverGames = Map.empty
    }

emptyRoundPlan :: PlayerRoundPlan
emptyRoundPlan =
  PlayerRoundPlan
    { planOrders = [],
      planTicketCount = 0
    }

createGame :: DefaultConfig -> ServerState -> (GameId, ServerState)
createGame config serverState =
  let config' = normalizeConfig config
      gameId = GameId (serverNextGameId serverState)
      runningGame =
        RunningGame
          { runningGameId = gameId,
            runningConfig = config',
            runningSeatCount = configPlayerCount config',
            runningHumanSeatCount = 0,
            runningNpcCount = configPlayerCount config',
            runningParticipants = [],
            runningStarted = True,
            runningRoundTimeLimitSeconds = Nothing,
            runningRoundDeadline = Nothing,
            runningSubmittedPlayers = [],
            runningRoundPlans = Map.empty,
            runningNextOrderId = 1,
            runningLatestTicketLottery = Nothing,
            runningTicketHistory = [],
            runningState = initialState config',
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

createLobby :: String -> Int -> Int -> Maybe Int -> ServerState -> (GameId, PlayerId, ServerState)
createLobby rawName requestedHumanSeats requestedNpcCount maybeRoundTimeLimit serverState =
  let humanSeats = normalizeHumanSeatCount requestedHumanSeats
      npcCount = normalizeNpcCount humanSeats requestedNpcCount
      totalSeats = humanSeats + npcCount
      config' =
        normalizeConfig
          defaultConfig
            { configPlayerCount = totalSeats
            }
      gameId = GameId (serverNextGameId serverState)
      playerId = PlayerId (serverNextPlayerId serverState)
      creator =
        HumanPlayer
          { humanPlayerId = playerId,
            humanPlayerName = normalizePlayerName rawName,
            humanPlayerEntityId = Nothing
          }
      runningGame =
        RunningGame
          { runningGameId = gameId,
            runningConfig = config',
            runningSeatCount = totalSeats,
            runningHumanSeatCount = humanSeats,
            runningNpcCount = npcCount,
            runningParticipants = [creator],
            runningStarted = False,
            runningRoundTimeLimitSeconds = normalizeRoundTimeLimit maybeRoundTimeLimit,
            runningRoundDeadline = Nothing,
            runningSubmittedPlayers = [],
            runningRoundPlans = Map.empty,
            runningNextOrderId = 1,
            runningLatestTicketLottery = Nothing,
            runningTicketHistory = [],
            runningState = initialState config',
            runningHistory = []
          }
      games' = Map.insert gameId runningGame (serverGames serverState)
   in
    ( gameId,
      playerId,
      serverState
        { serverNextGameId = serverNextGameId serverState + 1,
          serverNextPlayerId = serverNextPlayerId serverState + 1,
          serverGames = games'
        }
    )

joinGame :: GameId -> String -> ServerState -> Maybe (PlayerId, ServerState)
joinGame gameId rawName serverState = do
  runningGame <- getGame gameId serverState
  if runningStarted runningGame || length (runningParticipants runningGame) >= humanSeatCount runningGame
    then Nothing
    else
      let playerId = PlayerId (serverNextPlayerId serverState)
          participant =
            HumanPlayer
              { humanPlayerId = playerId,
                humanPlayerName = normalizePlayerName rawName,
                humanPlayerEntityId = Nothing
              }
          runningGame' =
            runningGame
              { runningParticipants = runningParticipants runningGame ++ [participant]
              }
          serverState' =
            serverState
              { serverNextPlayerId = serverNextPlayerId serverState + 1,
                serverGames = Map.insert gameId runningGame' (serverGames serverState)
              }
       in Just (playerId, serverState')

startGame :: UTCTime -> GameId -> ServerState -> Maybe ServerState
startGame now gameId serverState = do
  runningGame <- getGame gameId serverState
  if runningStarted runningGame || length (runningParticipants runningGame) /= humanSeatCount runningGame
    then Nothing
    else
      let startedGame = startRunningGame now runningGame
       in pure serverState {serverGames = Map.insert gameId startedGame (serverGames serverState)}

stageLimitOrder :: GameId -> PlayerId -> Side -> AssetId -> Int -> Int -> ServerState -> Maybe ServerState
stageLimitOrder gameId playerId side asset quantity price serverState = do
  runningGame <- getStartedGame gameId serverState
  participant <- getPlayer playerId runningGame
  if not (playerCanEditPlan playerId runningGame)
    then Nothing
    else do
      entityId' <- humanPlayerEntityId participant
      let normalizedQuantity = max 0 quantity
          normalizedPrice = max 0 price
      if normalizedQuantity <= 0 || normalizedPrice <= 0 || not (isAbstractAsset asset)
        then Nothing
        else
          let newOrder =
                Order
                  { orderId = OrderId (runningNextOrderId runningGame),
                    orderEntityId = entityId',
                    orderMarketId = MarketId 1,
                    orderSide = side,
                    orderBaseAsset = asset,
                    orderQuoteAsset = TLN001,
                    orderQuantity = normalizedQuantity,
                    orderLimitPrice = normalizedPrice
                  }
              currentPlan = getPlayerPlan playerId runningGame
              updatedPlan = currentPlan {planOrders = planOrders currentPlan ++ [newOrder]}
              updatedGame =
                runningGame
                  { runningRoundPlans = Map.insert playerId updatedPlan (runningRoundPlans runningGame),
                    runningNextOrderId = runningNextOrderId runningGame + 1
                  }
           in pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

removeStagedOrder :: GameId -> PlayerId -> OrderId -> ServerState -> Maybe ServerState
removeStagedOrder gameId playerId targetOrderId serverState = do
  runningGame <- getStartedGame gameId serverState
  _ <- getPlayer playerId runningGame
  if not (playerCanEditPlan playerId runningGame)
    then Nothing
    else
      let currentPlan = getPlayerPlan playerId runningGame
          filteredOrders = filter (\order -> orderId order /= targetOrderId) (planOrders currentPlan)
          updatedPlan = currentPlan {planOrders = filteredOrders}
          updatedGame =
            runningGame
              { runningRoundPlans = Map.insert playerId updatedPlan (runningRoundPlans runningGame)
              }
       in pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

setTicketCount :: GameId -> PlayerId -> Int -> ServerState -> Maybe ServerState
setTicketCount gameId playerId requestedCount serverState = do
  runningGame <- getStartedGame gameId serverState
  participant <- getPlayer playerId runningGame
  if not (playerCanEditPlan playerId runningGame)
    then Nothing
    else do
      entityId' <- humanPlayerEntityId participant
      let currentPlan = getPlayerPlan playerId runningGame
          maxTickets = balanceOf (gameHoldings (runningState runningGame)) entityId' TLN001
          normalizedCount = max 0 (min requestedCount maxTickets)
          updatedPlan = currentPlan {planTicketCount = normalizedCount}
          updatedGame =
            runningGame
              { runningRoundPlans = Map.insert playerId updatedPlan (runningRoundPlans runningGame)
              }
       in pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

submitTurn :: UTCTime -> Int -> GameId -> PlayerId -> ServerState -> Maybe ServerState
submitTurn now expectedRound gameId playerId serverState = do
  runningGame <- getStartedGame gameId serverState
  let syncedGame = syncTimedRound now runningGame
  participant <- getPlayer playerId syncedGame
  let syncedState =
        serverState
          { serverGames = Map.insert gameId syncedGame (serverGames serverState)
          }
      currentRound = gameRoundNumber (runningState syncedGame)
  if expectedRound /= currentRound
    then Just syncedState
    else
      let updatedGame =
            if playerCanSubmitTurn playerId syncedGame
              then markPlayerSubmitted now participant syncedGame
              else syncedGame
       in Just syncedState {serverGames = Map.insert gameId updatedGame (serverGames syncedState)}

syncGame :: UTCTime -> GameId -> ServerState -> Maybe ServerState
syncGame now gameId serverState = do
  runningGame <- getStartedGame gameId serverState
  let syncedGame = syncTimedRound now runningGame
  pure serverState {serverGames = Map.insert gameId syncedGame (serverGames serverState)}

listGames :: ServerState -> [RunningGame]
listGames =
  List.sortOn runningGameId . Map.elems . serverGames

getGame :: GameId -> ServerState -> Maybe RunningGame
getGame gameId = Map.lookup gameId . serverGames

getPlayer :: PlayerId -> RunningGame -> Maybe HumanPlayer
getPlayer playerId =
  find (\participant -> humanPlayerId participant == playerId) . runningParticipants

getPlayerPlan :: PlayerId -> RunningGame -> PlayerRoundPlan
getPlayerPlan playerId runningGame =
  Map.findWithDefault emptyRoundPlan playerId (runningRoundPlans runningGame)

resolveGame :: UTCTime -> GameId -> ServerState -> Maybe ServerState
resolveGame now gameId serverState = do
  runningGame <- getStartedGame gameId serverState
  let updatedGame = stepRunningGameAt now runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

resetGame :: UTCTime -> GameId -> ServerState -> Maybe ServerState
resetGame now gameId serverState = do
  runningGame <- getGame gameId serverState
  let resetGameState =
        if runningStarted runningGame
          then startRunningGame now runningGame
          else
            runningGame
              { runningState = initialState (runningConfig runningGame),
                runningHistory = [],
                runningSubmittedPlayers = [],
                runningRoundPlans = Map.empty,
                runningLatestTicketLottery = Nothing,
                runningTicketHistory = [],
                runningRoundDeadline = Nothing,
                runningNextOrderId = 1
              }
  pure serverState {serverGames = Map.insert gameId resetGameState (serverGames serverState)}

resetAllGames :: ServerState -> ServerState
resetAllGames _ = initialServerState

advanceGameBy :: Int -> GameId -> ServerState -> Maybe ServerState
advanceGameBy count gameId serverState = do
  runningGame <- getStartedGame gameId serverState
  let updatedGame = iterateGameSteps (max 0 count) runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

advanceGameToRound :: Int -> GameId -> ServerState -> Maybe ServerState
advanceGameToRound targetRound gameId serverState = do
  runningGame <- getStartedGame gameId serverState
  let updatedGame = iterateUntilTargetRound (max 1 targetRound) runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

advanceGameToEnd :: GameId -> ServerState -> Maybe ServerState
advanceGameToEnd gameId serverState = do
  runningGame <- getStartedGame gameId serverState
  let updatedGame = iterateUntilWinner maxAdvanceSteps runningGame
  pure serverState {serverGames = Map.insert gameId updatedGame (serverGames serverState)}

playerSubmittedTurn :: PlayerId -> RunningGame -> Bool
playerSubmittedTurn playerId runningGame =
  playerId `elem` runningSubmittedPlayers runningGame

playerCanEditPlan :: PlayerId -> RunningGame -> Bool
playerCanEditPlan playerId runningGame =
  runningStarted runningGame
    && not (isJust (gameWinner (runningState runningGame)))
    && playerId `elem` requiredSubmitters runningGame
    && not (playerSubmittedTurn playerId runningGame)

playerCanSubmitTurn :: PlayerId -> RunningGame -> Bool
playerCanSubmitTurn = playerCanEditPlan

requiredSubmitters :: RunningGame -> [PlayerId]
requiredSubmitters runningGame =
  [ humanPlayerId participant
    | participant <- runningParticipants runningGame,
      maybe False entityStillAlive (humanPlayerEntityId participant)
  ]
  where
    entityStillAlive entityId' =
      case Map.lookup entityId' (gameEntities (runningState runningGame)) of
        Nothing -> False
        Just entity -> entityAlive entity

roundSecondsRemaining :: UTCTime -> RunningGame -> Maybe Int
roundSecondsRemaining now runningGame =
  case runningRoundDeadline runningGame of
    Nothing -> Nothing
    Just deadline ->
      let remaining = ceiling (diffUTCTime deadline now :: NominalDiffTime)
       in Just (max 0 remaining)

maxAdvanceSteps :: Int
maxAdvanceSteps = 10000

humanSeatCount :: RunningGame -> Int
humanSeatCount runningGame = runningHumanSeatCount runningGame

normalizeConfig :: DefaultConfig -> DefaultConfig
normalizeConfig config =
  config {configPlayerCount = normalizeSeatCount (configPlayerCount config)}

normalizeHumanSeatCount :: Int -> Int
normalizeHumanSeatCount humanSeats = max 1 (min 6 humanSeats)

normalizeSeatCount :: Int -> Int
normalizeSeatCount seatCount = max 2 (min 6 seatCount)

normalizeNpcCount :: Int -> Int -> Int
normalizeNpcCount humanSeats npcCount =
  let minimumNpcCount = max 0 (2 - humanSeats)
      maximumNpcCount = max 0 (6 - humanSeats)
   in max minimumNpcCount (min maximumNpcCount npcCount)

normalizeRoundTimeLimit :: Maybe Int -> Maybe Int
normalizeRoundTimeLimit maybeSeconds =
  case maybeSeconds of
    Nothing -> Nothing
    Just seconds
      | seconds <= 0 -> Nothing
      | otherwise -> Just (min 300 seconds)

normalizePlayerName :: String -> String
normalizePlayerName rawName =
  let trimmed = trim rawName
   in if null trimmed
        then "Player"
        else trimmed

trim :: String -> String
trim =
  dropWhileEnd (`elem` [' ', '\t', '\n', '\r'])
    . dropWhile (`elem` [' ', '\t', '\n', '\r'])

getStartedGame :: GameId -> ServerState -> Maybe RunningGame
getStartedGame gameId serverState = do
  runningGame <- getGame gameId serverState
  if runningStarted runningGame
    then pure runningGame
    else Nothing

startRunningGame :: UTCTime -> RunningGame -> RunningGame
startRunningGame now runningGame =
  let (state', participants') = initializeStartedState runningGame
      startedGame =
        runningGame
          { runningStarted = True,
            runningParticipants = participants',
            runningState = state',
            runningHistory = [],
            runningSubmittedPlayers = [],
            runningRoundPlans = Map.empty,
            runningLatestTicketLottery = Nothing,
            runningTicketHistory = [],
            runningNextOrderId = 1
          }
   in resetRoundWindow now startedGame

initializeStartedState :: RunningGame -> (GameState, [HumanPlayer])
initializeStartedState runningGame =
  let state0 = initialState (runningConfig runningGame)
      assignments = zip [1 ..] (runningParticipants runningGame)
      state' = foldl' assignName state0 assignments
      participants' =
        [ participant {humanPlayerEntityId = Just (EntityId index)}
          | (index, participant) <- assignments
        ]
   in (state', participants')

assignName :: GameState -> (Int, HumanPlayer) -> GameState
assignName state (index, participant) =
  state
    { gameEntities =
        Map.adjust
          (\entity -> entity {entityName = humanPlayerName participant})
          (EntityId index)
          (gameEntities state)
    }

markPlayerSubmitted :: UTCTime -> HumanPlayer -> RunningGame -> RunningGame
markPlayerSubmitted now participant runningGame =
  let submittedPlayers =
        nub (runningSubmittedPlayers runningGame ++ [humanPlayerId participant])
      runningGame' = runningGame {runningSubmittedPlayers = submittedPlayers}
   in if allRequiredPlayersSubmitted runningGame'
        then stepRunningGameAt now runningGame'
        else runningGame'

allRequiredPlayersSubmitted :: RunningGame -> Bool
allRequiredPlayersSubmitted runningGame =
  let required = requiredSubmitters runningGame
   in not (null required) && all (`elem` runningSubmittedPlayers runningGame) required

syncTimedRound :: UTCTime -> RunningGame -> RunningGame
syncTimedRound now runningGame
  | not (runningStarted runningGame) = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise =
      case runningRoundDeadline runningGame of
        Nothing -> runningGame
        Just deadline ->
          if now >= deadline
            then stepRunningGameAt now runningGame
            else runningGame

resetRoundWindow :: UTCTime -> RunningGame -> RunningGame
resetRoundWindow now runningGame =
  let nextDeadline =
        case runningRoundTimeLimitSeconds runningGame of
          Nothing -> Nothing
          Just seconds ->
            if isJust (gameWinner (runningState runningGame))
              then Nothing
              else Just (addUTCTime (fromIntegral seconds) now)
   in
    runningGame
      { runningSubmittedPlayers = [],
        runningRoundPlans = Map.empty,
        runningRoundDeadline = nextDeadline
      }

stepRunningGameAt :: UTCTime -> RunningGame -> RunningGame
stepRunningGameAt now runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise =
      let submittedOrders = concatMap (planOrders . (`getPlayerPlan` runningGame)) (runningSubmittedPlayers runningGame)
          submittedTickets = collectSubmittedTickets runningGame
          (preStepState, maybeTicketResult) = applyTicketLottery submittedTickets (runningState runningGame)
          (state', events) = stepRound (runningConfig runningGame) (RoundInputs submittedOrders) preStepState
          reports = [report | RoundResolved report <- events]
          resolvedGame =
            runningGame
              { runningState = state',
                runningHistory = runningHistory runningGame ++ reports,
                runningLatestTicketLottery = maybeTicketResult,
                runningTicketHistory = runningTicketHistory runningGame ++ maybeToList maybeTicketResult
              }
       in resetRoundWindow now resolvedGame

collectSubmittedTickets :: RunningGame -> [(EntityId, Int)]
collectSubmittedTickets runningGame =
  foldr collectOne [] (runningSubmittedPlayers runningGame)
  where
    collectOne playerId acc =
      case getPlayer playerId runningGame >>= humanPlayerEntityId of
        Nothing -> acc
        Just entityId' ->
          let ticketCount = planTicketCount (getPlayerPlan playerId runningGame)
           in if ticketCount <= 0
                then acc
                else (entityId', ticketCount) : acc

applyTicketLottery :: [(EntityId, Int)] -> GameState -> (GameState, Maybe TicketLotteryResult)
applyTicketLottery purchases state =
  let normalizedPurchases = filter (\(_, count) -> count > 0) purchases
   in case normalizedPurchases of
        [] -> (state, Nothing)
        _ ->
          let governmentId = findGovernmentId state
              holdingsAfterPurchases =
                foldl'
                  (\holdings (entityId', count) ->
                     adjustBalance governmentId TLN001 count
                       . adjustBalance entityId' TLN001 (negate count)
                       $ holdings
                  )
                  (gameHoldings state)
                  normalizedPurchases
              entries =
                concatMap
                  (\(entityId', count) -> replicate count entityId')
                  normalizedPurchases
              (winner, seed') =
                case entries of
                  [] -> (Nothing, gameSeed state)
                  _ ->
                    let (selectedEntity, nextSeed') = drawFromList entries (gameSeed state)
                     in (Just selectedEntity, nextSeed')
              holdingsAfterRefund =
                case winner of
                  Nothing -> holdingsAfterPurchases
                  Just winnerId ->
                    adjustBalance governmentId TLN001 (-1)
                      . adjustBalance winnerId TLN001 1
                      $ holdingsAfterPurchases
              updatedState =
                state
                  { gameHoldings = holdingsAfterRefund,
                    gameSeed = seed'
                  }
              result =
                TicketLotteryResult
                  { ticketLotteryRoundNumber = gameRoundNumber state,
                    ticketLotteryPurchases = normalizedPurchases,
                    ticketLotteryWinner = winner
                  }
           in (updatedState, Just result)

iterateGameSteps :: Int -> RunningGame -> RunningGame
iterateGameSteps stepCount runningGame
  | stepCount <= 0 = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise = iterateGameSteps (stepCount - 1) (stepRunningGameAt noDeadlineTick runningGame)

iterateUntilTargetRound :: Int -> RunningGame -> RunningGame
iterateUntilTargetRound targetRound runningGame
  | gameRoundNumber (runningState runningGame) >= targetRound = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise = iterateUntilTargetRound targetRound (stepRunningGameAt noDeadlineTick runningGame)

iterateUntilWinner :: Int -> RunningGame -> RunningGame
iterateUntilWinner remainingSteps runningGame
  | remainingSteps <= 0 = runningGame
  | isJust (gameWinner (runningState runningGame)) = runningGame
  | otherwise = iterateUntilWinner (remainingSteps - 1) (stepRunningGameAt noDeadlineTick runningGame)

noDeadlineTick :: UTCTime
noDeadlineTick = read "1970-01-01 00:00:00 UTC"
