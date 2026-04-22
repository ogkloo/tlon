module Main where

import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Data.Time.Clock (UTCTime, addUTCTime)
import Tlon

main :: IO ()
main = do
  runTest "price-time matching supports partial fills" testPriceTimeMatching
  runTest "round rejects over-reserved inventory" testReserveValidation
  runTest "round settles fills then redeems and advances the table" testRoundSettlementAndRedemption
  runTest "unfilled remainder expires and grants are published for next round" testExpiryAndGrantReporting
  runTest "same entity cannot post opposite-side orders on one pair" testOpposingSideRejection
  runTest "lobby creation and joining assigns human seats on start" testLobbyFlow
  runTest "lobby can reserve npc seats and start without extra human joins" testLobbyNpcSeats
  runTest "players can stage orders and lottery tickets" testPlayerActionStaging
  runTest "submitted staged actions feed the resolved round" testSubmittedActionsResolve
  runTest "manual rounds wait for every player submission" testManualRoundSubmission
  runTest "timed rounds auto-resolve after the deadline" testTimedRoundSubmission
  runTest "reset all games clears the server state" testResetAllGames
  runTest "advance game by N rounds appends history" testAdvanceGameBy
  runTest "advance to round stops at target round" testAdvanceGameToRound
  runTest "advance to end stops at winner" testAdvanceGameToEnd
  runTest "advance to end respects the safety cap when no winner appears" testAdvanceGameToEndSafetyCap
  runTest "government wins when all mortals wash out" testGovernmentWin

runTest :: String -> IO () -> IO ()
runTest label assertion = do
  assertion
  putStrLn ("ok - " ++ label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else do
      putStrLn ("FAILED: " ++ label)
      putStrLn ("  expected: " ++ show expected)
      putStrLn ("  actual:   " ++ show actual)
      exitFailure

assertBool :: String -> Bool -> IO ()
assertBool label condition =
  if condition
    then pure ()
    else do
      putStrLn ("FAILED: " ++ label)
      exitFailure

testPriceTimeMatching :: IO ()
testPriceTimeMatching = do
  let market = MarketId 1
      sellerA = EntityId 1
      sellerB = EntityId 2
      buyer = EntityId 3
      orders =
        [ ValidatedOrder (Order (OrderId 1) sellerA market Sell TLN101 TLN001 2 2) 0,
          ValidatedOrder (Order (OrderId 2) sellerB market Sell TLN101 TLN001 2 1) 1,
          ValidatedOrder (Order (OrderId 3) buyer market Buy TLN101 TLN001 3 3) 2
        ]
      fills = matchRound PriceTimeFIFO orders
  assertEqual "fill count" 2 (length fills)
  case fills of
    firstFill : secondFill : [] -> do
      assertEqual "best ask fills first" (OrderId 2) (fillSellOrderId firstFill)
      assertEqual "first fill quantity" 2 (fillQuantity firstFill)
      assertEqual "second fill uses next ask" (OrderId 1) (fillSellOrderId secondFill)
      assertEqual "second fill quantity" 1 (fillQuantity secondFill)
    _ -> do
      putStrLn "FAILED: expected exactly two fills"
      exitFailure

testReserveValidation :: IO ()
testReserveValidation = do
  let config =
        defaultConfig
          { configPlayerCount = 2,
            configStartingAccessTokens = 5,
            configInitialBundleMax = 0,
            configRoundGrantQuantity = 0,
            configInitialSeed = 11
          }
      state0 = initialState config
      [player1, player2] = livingMortals state0
      holdings' =
        adjustBalance player2 TLN101 1 (gameHoldings state0)
      state1 = state0 {gameHoldings = holdings'}
      order1 = Order (OrderId 1) player1 (MarketId 1) Buy TLN101 TLN001 2 2
      order2 = Order (OrderId 2) player1 (MarketId 1) Buy TLN102 TLN001 2 2
      order3 = Order (OrderId 3) player2 (MarketId 1) Sell TLN101 TLN001 1 2
      (_, events) = stepRound config (RoundInputs [order1, order2, order3]) state1
      report = roundReportFrom events
  assertEqual "one order invalidated by reservation" 1 (length (reportInvalidOrders report))
  case reportInvalidOrders report of
    invalidOrder : [] ->
      assertEqual "invalid reason is insufficient inventory" InsufficientInventory (invalidReason invalidOrder)
    _ -> do
      putStrLn "FAILED: expected exactly one invalid order"
      exitFailure

testRoundSettlementAndRedemption :: IO ()
testRoundSettlementAndRedemption = do
  let config =
        defaultConfig
          { configPlayerCount = 2,
            configStartingAccessTokens = 0,
            configInitialBundleMax = 0,
            configRoundGrantQuantity = 0,
            configInitialSeed = 5
          }
      seller = Entity (EntityId 1) "Seller-1" PlayerEntity True
      buyer = Entity (EntityId 2) "Buyer-2" PlayerEntity True
      state0 =
        customState
          1
          17
          [seller, buyer]
          [ (entityId seller, [(TLN001, 1), (TLN101, 2)]),
            (entityId buyer, [(TLN001, 5)])
          ]
          (Map.fromList [(TLN101, 1), (TLN102, 2), (TLN103, 0)])
      orders =
        [ Order (OrderId 1) (entityId seller) (MarketId 1) Sell TLN101 TLN001 2 2,
          Order (OrderId 2) (entityId buyer) (MarketId 1) Buy TLN101 TLN001 1 2
        ]
      (state1, events) = stepRound config (RoundInputs orders) state0
      report = roundReportFrom events
      governmentId = EntityId 0
  assertEqual "one fill in the round" 1 (length (reportFills report))
  assertEqual "one expired seller remainder" [ExpiredOrder (OrderId 1) (entityId seller) 1] (reportExpiredOrders report)
  assertEqual
    "both players redeem TLN101 after settlement"
    [ Redemption (entityId seller) TLN101 1 1,
      Redemption (entityId buyer) TLN101 1 1
    ]
    (reportRedemptions report)
  assertEqual "round advances" 2 (gameRoundNumber state1)
  assertEqual "next round redemption table advances" (Map.fromList [(TLN101, 0), (TLN102, 1), (TLN103, 2)]) (gameRedemptionTable state1)
  assertEqual "seller keeps trade proceeds, redemption, and refund" 4 (balanceOf (gameHoldings state1) (entityId seller) TLN001)
  assertEqual "buyer spends two, redeems one, then pays stake" 3 (balanceOf (gameHoldings state1) (entityId buyer) TLN001)
  assertEqual "government quote balance reflects redemptions and survival net" 99 (balanceOf (gameHoldings state1) governmentId TLN001)
  assertEqual "buyer received one TLN101 then redeemed it" 0 (balanceOf (gameHoldings state1) (entityId buyer) TLN101)

testExpiryAndGrantReporting :: IO ()
testExpiryAndGrantReporting = do
  let config =
        defaultConfig
          { configPlayerCount = 2,
            configStartingAccessTokens = 0,
            configInitialBundleMax = 0,
            configRoundGrantQuantity = 1,
            configInitialSeed = 7
          }
      player1 = Entity (EntityId 1) "Orin-1" PlayerEntity True
      player2 = Entity (EntityId 2) "Moro-2" PlayerEntity True
      state0 =
        customState
          1
          29
          [player1, player2]
          [ (entityId player1, [(TLN001, 2), (TLN101, 1)]),
            (entityId player2, [(TLN001, 2)])
          ]
          (Map.fromList [(TLN101, 0), (TLN102, 1), (TLN103, 2)])
      order = Order (OrderId 1) (entityId player1) (MarketId 1) Sell TLN101 TLN001 1 2
      (state1, events) = stepRound config (RoundInputs [order]) state0
      report = roundReportFrom events
      surviving = livingMortals state1
  assertEqual "unmatched sell order expires" [ExpiredOrder (OrderId 1) (entityId player1) 1] (reportExpiredOrders report)
  assertEqual "no fills occurred" [] (reportFills report)
  assertEqual "both mortals survive" [entityId player1, entityId player2] surviving
  assertEqual "one grant per surviving mortal" 2 (length (reportNextRoundGrants report))
  assertEqual "reported next redemption table matches state" (reportNextRedemptionTable report) (gameRedemptionTable state1)
  assertEqual "grants are actually reflected in holdings" 2 (sum [balanceOf (gameHoldings state1) entityId' TLN101 + balanceOf (gameHoldings state1) entityId' TLN102 + balanceOf (gameHoldings state1) entityId' TLN103 | entityId' <- surviving])

testOpposingSideRejection :: IO ()
testOpposingSideRejection = do
  let config =
        defaultConfig
          { configPlayerCount = 1,
            configStartingAccessTokens = 5,
            configInitialBundleMax = 0,
            configRoundGrantQuantity = 0,
            configInitialSeed = 13
          }
      state0 = (initialState config) {gameHoldings = adjustBalance (EntityId 1) TLN101 1 (gameHoldings (initialState config))}
      order1 = Order (OrderId 1) (EntityId 1) (MarketId 1) Buy TLN101 TLN001 1 2
      order2 = Order (OrderId 2) (EntityId 1) (MarketId 1) Sell TLN101 TLN001 1 2
      (_, events) = stepRound config (RoundInputs [order1, order2]) state0
      report = roundReportFrom events
  case reportInvalidOrders report of
    [invalidOrder] ->
      assertEqual "opposite side rejected" OpposingSidesSamePair (invalidReason invalidOrder)
    _ -> do
      putStrLn "FAILED: expected exactly one invalid order"
      exitFailure

testLobbyFlow :: IO ()
testLobbyFlow = do
  let now = fixedTime
      (gameId, creatorId, serverState0) = createLobby "Alice" 3 0 Nothing initialServerState
      Just lobbyGame = getGame gameId serverState0
  assertBool "new lobby is not started yet" (not (runningStarted lobbyGame))
  assertEqual "creator is added to lobby" 1 (length (runningParticipants lobbyGame))
  assertEqual "creator keeps player id" (Just creatorId) (fmap humanPlayerId (getPlayer creatorId lobbyGame))

  let Just (bobId, serverState1) = joinGame gameId "Bob" serverState0
      Just (caraId, serverState2) = joinGame gameId "Cara" serverState1
      Just fullLobby = getGame gameId serverState2
  assertEqual "lobby fills all seats" 3 (length (runningParticipants fullLobby))
  assertEqual "full lobby rejects extra join" Nothing (joinGame gameId "Dana" serverState2)

  let Just serverState3 = startGame now gameId serverState2
      Just startedGame = getGame gameId serverState3
      playerNames =
        [ entityName (gameEntities (runningState startedGame) Map.! EntityId 1),
          entityName (gameEntities (runningState startedGame) Map.! EntityId 2),
          entityName (gameEntities (runningState startedGame) Map.! EntityId 3)
        ]
  assertBool "started game flips started flag" (runningStarted startedGame)
  assertEqual "human names are assigned to seats in join order" ["Alice", "Bob", "Cara"] playerNames
  assertEqual "creator receives first seat" (Just (EntityId 1)) (humanPlayerEntityId =<< getPlayer creatorId startedGame)
  assertEqual "second joiner receives second seat" (Just (EntityId 2)) (humanPlayerEntityId =<< getPlayer bobId startedGame)
  assertEqual "third joiner receives third seat" (Just (EntityId 3)) (humanPlayerEntityId =<< getPlayer caraId startedGame)

testLobbyNpcSeats :: IO ()
testLobbyNpcSeats = do
  let now = fixedTime
      (gameId, creatorId, serverState0) = createLobby "Alice" 1 2 Nothing initialServerState
      Just lobbyGame = getGame gameId serverState0
      Just serverState1 = startGame now gameId serverState0
      Just startedGame = getGame gameId serverState1
  assertEqual "one human seat is required" 1 (humanSeatCount lobbyGame)
  assertEqual "two npc seats are reserved" 2 (runningNpcCount lobbyGame)
  assertEqual "single human lobby can start with reserved npcs" True (runningStarted startedGame)
  assertEqual "creator keeps the first seat" (Just (EntityId 1)) (humanPlayerEntityId =<< getPlayer creatorId startedGame)
  assertBool "npc seat remains computer-controlled" (Map.lookup (EntityId 2) (gameEntities (runningState startedGame)) /= Nothing)

testPlayerActionStaging :: IO ()
testPlayerActionStaging = do
  let now = fixedTime
      (gameId, aliceId, serverState0) = createLobby "Alice" 2 0 Nothing initialServerState
      Just (_, serverState1) = joinGame gameId "Bob" serverState0
      Just serverState2 = startGame now gameId serverState1
      Just serverState3 = stageLimitOrder gameId aliceId Buy TLN101 2 3 serverState2
      Just serverState4 = setTicketCount gameId aliceId 2 serverState3
      Just runningGame = getGame gameId serverState4
      plan = getPlayerPlan aliceId runningGame
  assertEqual "one staged order is stored" 1 (length (planOrders plan))
  assertEqual "ticket count is stored" 2 (planTicketCount plan)
  case planOrders plan of
    [order] -> do
      assertEqual "staged order side" Buy (orderSide order)
      assertEqual "staged order asset" TLN101 (orderBaseAsset order)
      assertEqual "staged order quantity" 2 (orderQuantity order)
      assertEqual "staged order price" 3 (orderLimitPrice order)
    _ -> do
      putStrLn "FAILED: expected exactly one staged order"
      exitFailure

testSubmittedActionsResolve :: IO ()
testSubmittedActionsResolve = do
  let now = fixedTime
      (gameId, aliceId, serverState0) = createLobby "Alice" 2 0 Nothing initialServerState
      Just (bobId, serverState1) = joinGame gameId "Bob" serverState0
      Just serverState2 = startGame now gameId serverState1
      Just serverState3 = stageLimitOrder gameId aliceId Buy TLN101 1 2 serverState2
      Just serverState4 = setTicketCount gameId aliceId 1 serverState3
      Just serverState5 = stageLimitOrder gameId bobId Sell TLN101 1 2 serverState4
      Just serverState6 = submitTurn now 1 gameId aliceId serverState5
      Just serverState7 = submitTurn now 1 gameId bobId serverState6
      Just runningGame = getGame gameId serverState7
      Just report = gamePreviousReport (runningState runningGame)
  assertEqual "round advances after submitted actions resolve" 2 (gameRoundNumber (runningState runningGame))
  assertEqual "submitted staged orders reach the report" 2 (length (reportSubmittedOrders report))
  assertEqual "the staged match fills" 1 (length (reportFills report))
  assertBool "ticket lottery result is recorded" (runningLatestTicketLottery runningGame /= Nothing)
  case runningLatestTicketLottery runningGame of
    Just ticketResult ->
      assertEqual "one ticket purchase recorded" [(EntityId 1, 1)] (ticketLotteryPurchases ticketResult)
    Nothing -> do
      putStrLn "FAILED: expected a ticket lottery result"
      exitFailure

testManualRoundSubmission :: IO ()
testManualRoundSubmission = do
  let now = fixedTime
      (gameId, aliceId, serverState0) = createLobby "Alice" 2 0 Nothing initialServerState
      Just (bobId, serverState1) = joinGame gameId "Bob" serverState0
      Just serverState2 = startGame now gameId serverState1
      Just startedGame = getGame gameId serverState2
  assertEqual "no players submitted at round start" [] (runningSubmittedPlayers startedGame)

  let Just serverState3 = submitTurn now 1 gameId aliceId serverState2
      Just afterAlice = getGame gameId serverState3
  assertEqual "first submitter is recorded" [aliceId] (runningSubmittedPlayers afterAlice)
  assertEqual "round does not advance after one submit" 1 (gameRoundNumber (runningState afterAlice))

  let Just serverState4 = submitTurn now 1 gameId bobId serverState3
      Just afterBob = getGame gameId serverState4
  assertEqual "round advances after second submit" 2 (gameRoundNumber (runningState afterBob))
  assertEqual "submission list resets after resolution" [] (runningSubmittedPlayers afterBob)
  assertEqual "history records one resolved round" 1 (length (runningHistory afterBob))

testResetAllGames :: IO ()
testResetAllGames = do
  let (gameId, _, serverState0) = createLobby "Alice" 2 0 Nothing initialServerState
      Just (_, serverState1) = joinGame gameId "Bob" serverState0
      resetState = resetAllGames serverState1
  assertEqual "all games are cleared" [] (listGames resetState)
  assertEqual "game ids reset" 1 (serverNextGameId resetState)
  assertEqual "player ids reset" 1 (serverNextPlayerId resetState)

testTimedRoundSubmission :: IO ()
testTimedRoundSubmission = do
  let now = fixedTime
      later = addUTCTime 6 now
      (gameId, aliceId, serverState0) = createLobby "Alice" 2 0 (Just 5) initialServerState
      Just (_, serverState1) = joinGame gameId "Bob" serverState0
      Just serverState2 = startGame now gameId serverState1
      Just startedGame = getGame gameId serverState2
  assertEqual "timer is attached to the round" (Just 5) (runningRoundTimeLimitSeconds startedGame)
  assertBool "deadline is present for timed round" (runningRoundDeadline startedGame /= Nothing)

  let Just serverState3 = submitTurn now 1 gameId aliceId serverState2
      Just afterAlice = getGame gameId serverState3
  assertEqual "one player can lock in before deadline" [aliceId] (runningSubmittedPlayers afterAlice)
  assertEqual "round still waits before deadline" 1 (gameRoundNumber (runningState afterAlice))

  let Just serverState4 = syncGame later gameId serverState3
      Just afterDeadline = getGame gameId serverState4
  assertEqual "deadline advances the round without the second submit" 2 (gameRoundNumber (runningState afterDeadline))
  assertEqual "history records the auto-resolved round" 1 (length (runningHistory afterDeadline))
  assertEqual "submission list resets after the timed resolution" [] (runningSubmittedPlayers afterDeadline)

testAdvanceGameBy :: IO ()
testAdvanceGameBy = do
  let config =
        defaultConfig
          { configPlayerCount = 2,
            configRoundGrantQuantity = 0,
            configInitialSeed = 17
          }
      (gameId, serverState0) = createGame config initialServerState
      Just serverState1 = advanceGameBy 3 gameId serverState0
      Just runningGame = getGame gameId serverState1
  assertEqual "advance by three rounds updates round counter" 4 (gameRoundNumber (runningState runningGame))
  assertEqual "advance by three rounds appends three reports" 3 (length (runningHistory runningGame))

testAdvanceGameToRound :: IO ()
testAdvanceGameToRound = do
  let config =
        defaultConfig
          { configPlayerCount = 2,
            configRoundGrantQuantity = 0,
            configInitialSeed = 19
          }
      (gameId, serverState0) = createGame config initialServerState
      Just serverState1 = advanceGameToRound 6 gameId serverState0
      Just runningGame = getGame gameId serverState1
  assertEqual "advance to target round reaches target" 6 (gameRoundNumber (runningState runningGame))
  assertEqual "history length matches resolved rounds" 5 (length (runningHistory runningGame))

testAdvanceGameToEnd :: IO ()
testAdvanceGameToEnd = do
  let government = Entity (EntityId 0) "Government" GovernmentEntity True
      player = Entity (EntityId 1) "Orin-1" PlayerEntity True
      market =
        Market
          { marketId = MarketId 1,
            marketName = "Default Market",
            marketOwner = EntityId 0,
            marketPairs = [(asset, TLN001) | asset <- allAbstractAssets]
          }
      runningGame =
        RunningGame
          { runningGameId = GameId 1,
            runningConfig = defaultConfig {configRoundGrantQuantity = 0},
            runningSeatCount = 1,
            runningHumanSeatCount = 0,
            runningNpcCount = 1,
            runningParticipants = [],
            runningStarted = True,
            runningRoundTimeLimitSeconds = Nothing,
            runningRoundDeadline = Nothing,
            runningSubmittedPlayers = [],
            runningRoundPlans = Map.empty,
            runningNextOrderId = 1,
            runningLatestTicketLottery = Nothing,
            runningTicketHistory = [],
            runningState =
              GameState
                { gameRoundNumber = 1,
                  gameMatchingPolicy = PriceTimeFIFO,
                  gameSeed = 3,
                  gameEntities = Map.fromList [(EntityId 0, government), (EntityId 1, player)],
                  gameMarkets = Map.fromList [(MarketId 1, market)],
                  gameHoldings = Map.fromList [(EntityId 0, Map.fromList [(TLN001, 100)]), (EntityId 1, Map.empty)],
                  gameRedemptionTable = Map.fromList [(TLN101, 0), (TLN102, 0), (TLN103, 0)],
                  gamePreviousReport = Nothing,
                  gameWinner = Nothing
                },
            runningHistory = []
          }
      serverState0 =
        ServerState
          { serverNextGameId = 2,
            serverNextPlayerId = 1,
            serverGames = Map.fromList [(GameId 1, runningGame)]
          }
      Just serverState1 = advanceGameToEnd (GameId 1) serverState0
      Just finishedGame = getGame (GameId 1) serverState1
  assertEqual "advance to end declares government winner" (Just (EntityId 0)) (gameWinner (runningState finishedGame))
  assertEqual "advance to end stops after one eliminating round" 1 (length (runningHistory finishedGame))

testAdvanceGameToEndSafetyCap :: IO ()
testAdvanceGameToEndSafetyCap = do
  let player1 = Entity (EntityId 1) "Orin-1" PlayerEntity True
      player2 = Entity (EntityId 2) "Moro-2" PlayerEntity True
      state0 =
        customState
          1
          23
          [player1, player2]
          [ (entityId player1, [(TLN001, 20000)]),
            (entityId player2, [(TLN001, 20000)])
          ]
          (Map.fromList [(TLN101, 0), (TLN102, 0), (TLN103, 0)])
      runningGame =
        RunningGame
          { runningGameId = GameId 1,
            runningConfig = defaultConfig {configRoundGrantQuantity = 0},
            runningSeatCount = 2,
            runningHumanSeatCount = 0,
            runningNpcCount = 2,
            runningParticipants = [],
            runningStarted = True,
            runningRoundTimeLimitSeconds = Nothing,
            runningRoundDeadline = Nothing,
            runningSubmittedPlayers = [],
            runningRoundPlans = Map.empty,
            runningNextOrderId = 1,
            runningLatestTicketLottery = Nothing,
            runningTicketHistory = [],
            runningState = state0,
            runningHistory = []
          }
      serverState0 =
        ServerState
          { serverNextGameId = 2,
            serverNextPlayerId = 1,
            serverGames = Map.fromList [(GameId 1, runningGame)]
          }
      Just serverState1 = advanceGameToEnd (GameId 1) serverState0
      Just cappedGame = getGame (GameId 1) serverState1
  assertEqual "advance to end stops at the safety cap" 10001 (gameRoundNumber (runningState cappedGame))
  assertEqual "advance to end leaves winner unset when cap is reached" Nothing (gameWinner (runningState cappedGame))
  assertEqual "advance to end records one report per capped step" 10000 (length (runningHistory cappedGame))

testGovernmentWin :: IO ()
testGovernmentWin = do
  let government = Entity (EntityId 0) "Government" GovernmentEntity True
      player1 = Entity (EntityId 1) "Orin-1" PlayerEntity True
      player2 = Entity (EntityId 2) "Moro-2" PlayerEntity True
      market =
        Market
          { marketId = MarketId 1,
            marketName = "Default Market",
            marketOwner = EntityId 0,
            marketPairs = [(asset, TLN001) | asset <- allAbstractAssets]
          }
      holdings =
        Map.fromList
          [ (EntityId 0, Map.fromList [(TLN001, 100), (TLN101, 100), (TLN102, 100), (TLN103, 100)]),
            (EntityId 1, Map.empty),
            (EntityId 2, Map.empty)
          ]
      state0 =
        GameState
          { gameRoundNumber = 1,
            gameMatchingPolicy = PriceTimeFIFO,
            gameSeed = 3,
            gameEntities = Map.fromList [(EntityId 0, government), (EntityId 1, player1), (EntityId 2, player2)],
            gameMarkets = Map.fromList [(MarketId 1, market)],
            gameHoldings = holdings,
            gameRedemptionTable = Map.fromList [(TLN101, 0), (TLN102, 0), (TLN103, 0)],
            gamePreviousReport = Nothing,
            gameWinner = Nothing
          }
      (state1, _) = stepRound defaultConfig {configRoundGrantQuantity = 0} (RoundInputs []) state0
  assertEqual "government wins when no mortal can survive" (Just (EntityId 0)) (gameWinner state1)
  assertBool "player one eliminated" (not (entityAlive (gameEntities state1 Map.! EntityId 1)))
  assertBool "player two eliminated" (not (entityAlive (gameEntities state1 Map.! EntityId 2)))

roundReportFrom :: [GameEvent] -> RoundReport
roundReportFrom events =
  case events of
    [RoundResolved report] -> report
    _ -> error "Expected a single round report."

customState :: Int -> Int -> [Entity] -> [(EntityId, [(AssetId, Quantity)])] -> Map.Map AssetId Quantity -> GameState
customState roundNumber seed players playerHoldings redemptionTable =
  let government = Entity (EntityId 0) "Government" GovernmentEntity True
      market =
        Market
          { marketId = MarketId 1,
            marketName = "Default Market",
            marketOwner = EntityId 0,
            marketPairs = [(asset, TLN001) | asset <- allAbstractAssets]
          }
      entities =
        Map.fromList
          ( (entityId government, government) :
            [(entityId player, player) | player <- players]
          )
      holdings =
        Map.fromList
          ( (EntityId 0, Map.fromList [(TLN001, 100), (TLN101, 100), (TLN102, 100), (TLN103, 100)]) :
            [(entityId', Map.fromList ledger) | (entityId', ledger) <- playerHoldings]
          )
   in GameState
        { gameRoundNumber = roundNumber,
          gameMatchingPolicy = PriceTimeFIFO,
          gameSeed = seed,
          gameEntities = entities,
          gameMarkets = Map.fromList [(MarketId 1, market)],
          gameHoldings = holdings,
          gameRedemptionTable = redemptionTable,
          gamePreviousReport = Nothing,
          gameWinner = Nothing
        }

fixedTime :: UTCTime
fixedTime = read "2026-04-21 12:00:00 UTC"
