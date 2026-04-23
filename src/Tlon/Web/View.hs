{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.View (
    renderGamePage,
    renderGameShell,
    renderLobbyShell,
    renderIndexPage,
    renderNotFoundPage,
)
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import Lucid
import Lucid.Base (makeAttribute)
import Tlon.Core.Event
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Web.State

renderIndexPage :: Bool -> [RunningGame] -> Html ()
renderIndexPage debugEnabled games =
    layout "Tlon" $ do
        section_ [class_ "band hero"] $ do
            div_ [class_ "wrap hero-grid"] $ do
                div_ $ do
                    h1_ "Tlon"
                    p_ [class_ "lede"] "Start a lobby, share the player links, and gate each round on player submissions."
                div_ [class_ "panel"] $ do
                    h2_ "Start Match"
                    form_ [method_ "post", action_ "/games", class_ "control-stack"] $ do
                        label_ [class_ "control-label"] $ do
                            span_ "Your Name"
                            input_ [type_ "text", name_ "playerName", value_ "Player 1", maxlength_ "32"]
                        label_ [class_ "control-label"] $ do
                            span_ "Human Players"
                            input_ [type_ "number", name_ "playerCount", min_ "1", max_ "6", value_ "2"]
                        label_ [class_ "control-label"] $ do
                            span_ "NPCs"
                            input_ [type_ "number", name_ "npcCount", min_ "0", max_ "5", value_ "0"]
                        label_ [class_ "control-label"] $ do
                            span_ "Seconds Per Round"
                            input_ [type_ "number", name_ "roundTimeLimitSeconds", min_ "0", max_ "300", value_ "0"]
                        p_ [class_ "control-note"] "Set to 0 for manual submissions only."
                        button_ [type_ "submit"] "Create Lobby"
        section_ [class_ "band"] $ do
            div_ [class_ "wrap"] $ do
                h2_ "Games"
                if null games
                    then p_ "No games yet."
                    else table_ [class_ "data-table"] $ do
                        thead_ $
                            tr_ $ do
                                th_ "Game"
                                th_ "Phase"
                                th_ "Seats"
                                th_ "Timer"
                                th_ "Round"
                                th_ "Status"
                                th_ ""
                        tbody_ $
                            mapM_ renderGameRow games
                if debugEnabled
                    then div_ [class_ "panel"] $ do
                        h2_ "Debug"
                        p_ [class_ "control-note"] "Clears every lobby and running game."
                        form_ [method_ "post", action_ "/debug/reset-all", class_ "inline-form"] $
                            button_ [type_ "submit", class_ "secondary-button", makeAttribute "onclick" "return window.confirm('Reset all games?');"] "Reset All Games"
                    else mempty

renderGamePage :: Bool -> RunningGame -> Maybe HumanPlayer -> Maybe Int -> Html ()
renderGamePage debugEnabled runningGame maybePlayer maybeSecondsRemaining =
    let gameLabel = showGameId (runningGameId runningGame)
     in layout ("Tlon Game " <> Text.pack gameLabel) $ do
            if runningStarted runningGame
                then renderGameShell debugEnabled runningGame maybePlayer maybeSecondsRemaining
                else renderLobbyShell debugEnabled runningGame maybePlayer
            if debugEnabled
                then renderAgentationMount runningGame maybePlayer
                else mempty

renderGameShell :: Bool -> RunningGame -> Maybe HumanPlayer -> Maybe Int -> Html ()
renderGameShell debugEnabled runningGame maybePlayer maybeSecondsRemaining =
    let state = runningState runningGame
        report = gamePreviousReport state
        gameLabel = showGameId (runningGameId runningGame)
     in div_
            [ id_ "game-shell"
            , makeAttribute "data-game-id" (Text.pack gameLabel)
            , makeAttribute "data-round-number" (Text.pack (show (gameRoundNumber state)))
            , makeAttribute "data-has-winner" (if gameWinner state == Nothing then "false" else "true")
            , makeAttribute "data-player-id" (maybe "" (Text.pack . showPlayerId . humanPlayerId) maybePlayer)
            , makeAttribute "data-sync-path" (pathText (scopedBasePath runningGame maybePlayer ++ "/sync"))
            ]
            $ do
                section_ [class_ "band hero"] $ do
                    div_ [class_ "wrap"] $ do
                        div_ [class_ "topbar"] $ do
                            div_ $ do
                                h1_ ("Game " <> toHtml gameLabel)
                                p_ [class_ "lede"] $
                                    "Round "
                                        <> toHtml (show (gameRoundNumber state))
                                        <> " | "
                                        <> winnerLabel (gameWinner state)
                                renderCurrentPlayerNote maybePlayer
                                p_ [class_ "timer-note"] (toHtml (roundPhaseSummary runningGame maybeSecondsRemaining))
                            div_ [class_ "actions"] $ do
                                a_ [href_ "/", class_ "button secondary"] "Games"
                                renderPrimaryAction runningGame maybePlayer
                                form_
                                    [ method_ "post"
                                    , action_ (pathText (scopedBasePath runningGame maybePlayer ++ "/reset"))
                                    , class_ "inline-form reset-form"
                                    , makeAttribute "hx-post" (pathText (scopedBasePath runningGame maybePlayer ++ "/reset"))
                                    , makeAttribute "hx-target" "#game-shell"
                                    , makeAttribute "hx-swap" "outerHTML"
                                    , makeAttribute "hx-confirm" "Reset this game?"
                                    ]
                                    $ button_ [type_ "submit", class_ "secondary-button"] "Reset Game"
                section_ [class_ "band"] $ do
                    div_ [class_ "wrap section-stack"] $ do
                        div_ [class_ "panel"] $ do
                            h2_ "Latest Report"
                            maybe (p_ "No rounds resolved yet.") renderReportSummary report
                        case maybePlayer of
                            Nothing -> mempty
                            Just participant ->
                                div_ [class_ "panel"] $ do
                                    h2_ "Your Inventory"
                                    playerInventoryPanel runningGame participant
                        div_ [class_ "panel"] $ do
                            h2_ "Players"
                            participantsTable runningGame
                        case maybePlayer of
                            Nothing -> mempty
                            Just participant ->
                                div_ [class_ "panel"] $ do
                                    h2_ "Your Actions"
                                    playerActionsPanel runningGame participant
                        div_ [class_ "panel"] $ do
                            h2_ "Round Status"
                            submissionStatusPanel runningGame maybePlayer maybeSecondsRemaining
                        div_ [class_ "panel"] $ do
                            h2_ "Markets"
                            marketsPanel runningGame maybePlayer
                        div_ [class_ "panel"] $ do
                            h2_ "Lottery Menu"
                            lotteryMenuPanel state
                        div_ [class_ "panel"] $ do
                            h2_ "Overview"
                            overviewList runningGame
                        if isJust maybePlayer || not debugEnabled
                            then mempty
                            else div_ [class_ "panel"] $ do
                                h2_ "Debug Controls"
                                roundControls runningGame
                section_ [class_ "band"] $ do
                    div_ [class_ "wrap"] $ do
                        h2_ "Holdings"
                        table_ [class_ "data-table"] $ do
                            thead_ $
                                tr_ $ do
                                    th_ "Entity"
                                    th_ "Kind"
                                    th_ "Status"
                                    th_ "TLN-001"
                                    th_ "TLN-101"
                                    th_ "TLN-102"
                                    th_ "TLN-103"
                            tbody_ $ mapM_ (renderEntityRow state) (Map.elems (gameEntities state))
                section_ [class_ "band"] $ do
                    div_ [class_ "wrap section-stack"] $ do
                        div_ [class_ "panel"] $ do
                            h2_ "Lottery Results"
                            case gamePreviousReport state of
                                Nothing -> p_ "No lottery results last round."
                                Just report -> renderLotteryResultsSummary report
                        div_ [class_ "panel"] $ do
                            h2_ "History"
                            historyList (runningHistory runningGame)

renderLobbyShell :: Bool -> RunningGame -> Maybe HumanPlayer -> Html ()
renderLobbyShell debugEnabled runningGame maybePlayer =
    let gameLabel = showGameId (runningGameId runningGame)
        playerCount = length (runningParticipants runningGame)
        seatsOpen = humanSeatCount runningGame - playerCount
     in div_
            [ id_ "game-shell"
            , makeAttribute "data-sync-path" (pathText (scopedBasePath runningGame maybePlayer ++ "/sync"))
            ]
            $ do
                section_ [class_ "band hero"] $ do
                    div_ [class_ "wrap"] $ do
                        div_ [class_ "topbar"] $ do
                            div_ $ do
                                h1_ ("Game " <> toHtml gameLabel)
                                p_ [class_ "lede"] $
                                    "Lobby "
                                        <> toHtml (show playerCount)
                                        <> "/"
                                        <> toHtml (show (humanSeatCount runningGame))
                                        <> " humans ready"
                                        <> toHtml (npcSummarySuffix runningGame)
                                renderCurrentPlayerNote maybePlayer
                                p_ [class_ "timer-note"] (toHtml (lobbyTimerLabel runningGame))
                            div_ [class_ "actions"] $ do
                                a_ [href_ "/", class_ "button secondary"] "Games"
                                if seatsOpen == 0
                                    then
                                        form_
                                            [ method_ "post"
                                            , action_ (pathText (scopedBasePath runningGame maybePlayer ++ "/start"))
                                            , class_ "inline-form"
                                            ]
                                            $ button_ [type_ "submit"] "Start Match"
                                    else
                                        button_ [type_ "button", makeAttribute "disabled" "disabled", class_ "secondary-button"] "Waiting For Players"
                section_ [class_ "band"] $ do
                    div_ [class_ "wrap section-stack"] $ do
                        div_ [class_ "panel"] $ do
                            h2_ "Table"
                            participantsTable runningGame
                        div_ [class_ "panel"] $ do
                            h2_ "Join"
                            joinPanel runningGame maybePlayer
                        div_ [class_ "panel"] $ do
                            h2_ "Status"
                            ul_ [class_ "compact-list"] $ do
                                li_ ("Human seats filled: " <> toHtml (show playerCount) <> " / " <> toHtml (show (humanSeatCount runningGame)))
                                li_ ("NPC seats: " <> toHtml (show (runningNpcCount runningGame)))
                                li_ ("Open seats: " <> toHtml (show (max 0 seatsOpen)))
                                li_ (toHtml (lobbyTimerLabel runningGame))
                                li_ $
                                    if seatsOpen == 0
                                        then "Ready to start."
                                        else "Waiting for more players."
                        if debugEnabled
                            then div_ [class_ "panel"] $ do
                                h2_ "Debug"
                                p_ [class_ "control-note"] "Clears every lobby and running game."
                                form_ [method_ "post", action_ "/debug/reset-all", class_ "inline-form"] $
                                    button_ [type_ "submit", class_ "secondary-button", makeAttribute "onclick" "return window.confirm('Reset all games?');"] "Reset All Games"
                            else mempty

renderNotFoundPage :: Html ()
renderNotFoundPage =
    layout "Not Found" $
        section_ [class_ "band"] $
            div_ [class_ "wrap"] $ do
                h1_ "Not Found"
                p_ "That game does not exist."
                a_ [href_ "/", class_ "button secondary"] "Back"

layout :: Text.Text -> Html () -> Html ()
layout pageTitle bodyContent =
    doctypehtml_ $ do
        head_ $ do
            title_ (toHtml pageTitle)
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            with
                (script_ reloadScript)
                [type_ "text/javascript"]
            with
                (script_ syncScript)
                [type_ "text/javascript"]
            with
                (script_ ("" :: Text.Text))
                [src_ "/static/htmx.min.js"]
            style_ css
        body_ bodyContent

css :: Text.Text
css =
    Text.unlines
        [ "html { font-family: Inter, system-ui, sans-serif; background: #f4f1eb; color: #141414; }"
        , "body { margin: 0; }"
        , ".band { padding: 24px 0; border-top: 1px solid #d9d2c7; }"
        , ".hero { background: #efe6d2; border-top: 0; }"
        , ".wrap { max-width: 1120px; margin: 0 auto; padding: 0 20px; }"
        , ".hero-grid { display: grid; grid-template-columns: minmax(0, 1.4fr) minmax(320px, 0.8fr); gap: 24px; align-items: start; }"
        , ".grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 16px; }"
        , ".section-stack { display: grid; grid-template-columns: minmax(0, 1fr); gap: 16px; }"
        , ".panel { background: #fbfaf7; border: 1px solid #d9d2c7; border-radius: 8px; padding: 16px; }"
        , ".topbar { display: flex; justify-content: space-between; gap: 16px; align-items: end; flex-wrap: wrap; }"
        , ".actions { display: flex; gap: 12px; align-items: center; flex-wrap: wrap; }"
        , ".control-stack { display: grid; gap: 12px; }"
        , ".control-row { display: grid; grid-template-columns: minmax(0, 1fr) auto; gap: 10px; align-items: end; }"
        , ".control-row.triple { grid-template-columns: minmax(0, 1fr) auto auto; }"
        , ".control-label { display: grid; gap: 6px; color: #50483d; }"
        , ".control-label span { font-size: 14px; }"
        , ".control-label input { width: 100%; box-sizing: border-box; border: 1px solid #cdbfae; border-radius: 6px; padding: 10px 12px; font: inherit; background: #fff; }"
        , ".control-note { margin: -4px 0 0; font-size: 14px; color: #6a6155; }"
        , ".player-note { margin: 8px 0 0; color: #2c5a51; font-weight: 600; }"
        , ".timer-note { margin: 8px 0 0; color: #6a6155; }"
        , ".share-link { word-break: break-all; font-family: ui-monospace, SFMono-Regular, monospace; background: #f3ede2; border: 1px solid #d9d2c7; border-radius: 6px; padding: 10px 12px; }"
        , ".status-line { margin: 0 0 10px; color: #50483d; }"
        , ".status-line strong { color: #141414; }"
        , ".actions-list { display: grid; gap: 10px; }"
        , ".inline-fieldset { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr) minmax(0, 1fr) auto; gap: 10px; align-items: end; }"
        , ".inline-fieldset.compact { grid-template-columns: minmax(0, 1fr) auto; }"
        , ".inline-fieldset select, .inline-fieldset input { width: 100%; box-sizing: border-box; border: 1px solid #cdbfae; border-radius: 6px; padding: 10px 12px; font: inherit; background: #fff; }"
        , ".lede { margin: 8px 0 0; color: #50483d; }"
        , ".button, button { display: inline-block; background: #1f6f5f; color: #fff; border: 0; border-radius: 6px; padding: 10px 14px; font: inherit; text-decoration: none; cursor: pointer; }"
        , ".button.secondary { background: #45332b; }"
        , ".secondary-button { background: #7a3f2a; color: #fff; }"
        , "button[disabled] { cursor: default; opacity: 0.55; }"
        , ".inline-form { margin: 0; }"
        , ".resolve-form button { display: inline-flex; gap: 8px; align-items: center; }"
        , ".resolve-form.htmx-request button { opacity: 0.72; }"
        , ".resolve-indicator { display: none; }"
        , ".resolve-form.htmx-request .resolve-indicator { display: inline; }"
        , ".market-card + .market-card { border-top: 1px solid #e6e0d6; padding-top: 12px; margin-top: 12px; }"
        , "h1, h2, h3 { margin: 0 0 12px; }"
        , "p, ul { margin: 0 0 12px; }"
        , ".data-table { width: 100%; border-collapse: collapse; background: #fbfaf7; border: 1px solid #d9d2c7; border-radius: 8px; overflow: hidden; }"
        , ".data-table th, .data-table td { text-align: left; padding: 10px 12px; border-bottom: 1px solid #e6e0d6; vertical-align: top; }"
        , ".data-table thead { background: #e6ded0; }"
        , ".data-table tr:last-child td { border-bottom: 0; }"
        , ".tag { display: inline-block; padding: 2px 8px; border-radius: 999px; background: #ddd3c6; }"
        , ".tag.dead { background: #3f2d28; color: #fff; }"
        , ".compact-list { padding-left: 18px; }"
        , ".compact-list li { margin-bottom: 6px; }"
        , "@media (max-width: 860px) { .hero-grid { grid-template-columns: 1fr; } .control-row.triple { grid-template-columns: 1fr; } .inline-fieldset, .inline-fieldset.compact { grid-template-columns: 1fr; } }"
        ]

reloadScript :: Text.Text
reloadScript =
    Text.unlines
        [ "(function () {"
        , "  var currentToken = null;"
        , "  var started = false;"
        , "  function poll() {"
        , "    fetch('/dev/reload-token', { cache: 'no-store' })"
        , "      .then(function (response) { return response.ok ? response.text() : null; })"
        , "      .then(function (token) {"
        , "        if (!token) return;"
        , "        if (!started) {"
        , "          currentToken = token;"
        , "          started = true;"
        , "          return;"
        , "        }"
        , "        if (currentToken !== token) {"
        , "          window.location.reload();"
        , "        }"
        , "      })"
        , "      .catch(function () {})"
        , "      .finally(function () { window.setTimeout(poll, 1000); });"
        , "  }"
        , "  window.setTimeout(poll, 1000);"
        , "})();"
        ]

syncScript :: Text.Text
syncScript =
    Text.unlines
        [ "(function () {"
        , "  var pollInFlight = false;"
        , "  var interactionUntil = 0;"
        , "  var interactionWindowMs = 1500;"
        , "  function getShell() { return document.getElementById('game-shell'); }"
        , "  function isEditableElement(node) {"
        , "    return !!(node && node.closest && node.closest('input, select, textarea, [contenteditable=\"true\"]'));"
        , "  }"
        , "  function markInteraction() {"
        , "    interactionUntil = Date.now() + interactionWindowMs;"
        , "  }"
        , "  function userIsEditing() {"
        , "    var active = document.activeElement;"
        , "    return isEditableElement(active) || Date.now() < interactionUntil;"
        , "  }"
        , "  function htmxBusy() {"
        , "    return !!document.querySelector('.htmx-request');"
        , "  }"
        , "  function replaceShell(html) {"
        , "    var shell = getShell();"
        , "    if (shell && shell.outerHTML !== html) { shell.outerHTML = html; }"
        , "  }"
        , "  function poll() {"
        , "    var shell = getShell();"
        , "    if (!shell) { window.setTimeout(poll, 1000); return; }"
        , "    var syncPath = shell.dataset.syncPath;"
        , "    if (!syncPath || pollInFlight || htmxBusy() || userIsEditing()) { window.setTimeout(poll, 1000); return; }"
        , "    pollInFlight = true;"
        , "    fetch(syncPath, { headers: { 'HX-Request': 'true' }, cache: 'no-store' })"
        , "      .then(function (response) { return response.ok ? response.text() : null; })"
        , "      .then(function (html) { if (html && !htmxBusy() && !userIsEditing()) { replaceShell(html); } })"
        , "      .catch(function () {})"
        , "      .finally(function () { pollInFlight = false; window.setTimeout(poll, 1000); });"
        , "  }"
        , "  document.addEventListener('focusin', function (event) { if (isEditableElement(event.target)) { markInteraction(); } });"
        , "  document.addEventListener('input', function (event) { if (isEditableElement(event.target)) { markInteraction(); } });"
        , "  document.addEventListener('change', function (event) { if (isEditableElement(event.target)) { markInteraction(); } });"
        , "  document.addEventListener('pointerdown', function (event) { if (isEditableElement(event.target)) { markInteraction(); } });"
        , "  window.setTimeout(poll, 1000);"
        , "})();"
        ]

renderGameRow :: RunningGame -> Html ()
renderGameRow runningGame =
    let state = runningState runningGame
        phaseLabel :: String
        phaseLabel = if runningStarted runningGame then "Live" else "Lobby"
        statusLabel :: String
        statusLabel =
            if runningStarted runningGame
                then winnerText (gameWinner state)
                else "waiting"
     in tr_ $ do
            td_ ("Game " <> toHtml (showGameId (runningGameId runningGame)))
            td_ (toHtml phaseLabel)
            td_ (toHtml (humanSeatSummary runningGame))
            td_ (toHtml (timerLabel runningGame))
            td_ $
                if runningStarted runningGame
                    then toHtml (show (gameRoundNumber state))
                    else toHtml ("-" :: String)
            td_ (toHtml statusLabel)
            td_ $
                a_ [href_ (pathText ("/games/" ++ showGameId (runningGameId runningGame))), class_ "button secondary"] "Open"

renderEntityRow :: GameState -> Entity -> Html ()
renderEntityRow state entity =
    let entityLedger = Map.findWithDefault Map.empty (entityId entity) (gameHoldings state)
        renderAsset :: AssetId -> Html ()
        renderAsset asset = td_ (toHtml (show (Map.findWithDefault 0 (assetSeriesId asset) entityLedger)))
     in tr_ $ do
            td_ (toHtml (entityName entity))
            td_ (toHtml (show (entityKind entity)))
            td_ $
                if entityAlive entity
                    then span_ [class_ "tag"] "active"
                    else span_ [class_ "tag dead"] "eliminated"
            renderAsset TLN001
            renderAsset TLN101
            renderAsset TLN102
            renderAsset TLN103

participantsTable :: RunningGame -> Html ()
participantsTable runningGame =
    table_ [class_ "data-table"] $ do
        thead_ $
            tr_ $ do
                th_ "Seat"
                th_ "Player"
                th_ "Link"
                if runningStarted runningGame
                    then th_ "Round"
                    else mempty
        tbody_ $ do
            mapM_ renderParticipantSeat [1 .. runningSeatCount runningGame]
  where
    participants = runningParticipants runningGame
    renderParticipantSeat :: Int -> Html ()
    renderParticipantSeat seatIndex =
        if seatIndex > humanSeatCount runningGame
            then renderNpcSeat seatIndex
            else case drop (seatIndex - 1) participants of
                participant : _ ->
                    tr_ $ do
                        td_ (toHtml ("P" ++ show seatIndex))
                        td_ (toHtml (humanPlayerName participant))
                        td_ $
                            code_ (toHtml ("/games/" ++ showGameId (runningGameId runningGame) ++ "/players/" ++ showPlayerId (humanPlayerId participant)))
                        if runningStarted runningGame
                            then td_ (toHtml (playerRoundState participant runningGame))
                            else mempty
                [] ->
                    tr_ $ do
                        td_ (toHtml ("P" ++ show seatIndex))
                        td_ [colspan_ (if runningStarted runningGame then "3" else "2")] "Open seat"
    renderNpcSeat :: Int -> Html ()
    renderNpcSeat seatIndex =
        tr_ $ do
            td_ (toHtml ("P" ++ show seatIndex))
            td_ (toHtml (npcSeatName seatIndex runningGame))
            td_ "-"
            if runningStarted runningGame
                then td_ "npc"
                else mempty

joinPanel :: RunningGame -> Maybe HumanPlayer -> Html ()
joinPanel runningGame maybePlayer =
    case maybePlayer of
        Just participant -> do
            p_ ("Joined as " <> toHtml (humanPlayerName participant) <> ".")
            p_ "Share this player link with the person using this seat:"
            p_ [class_ "share-link"] $
                toHtml ("/games/" ++ showGameId (runningGameId runningGame) ++ "/players/" ++ showPlayerId (humanPlayerId participant))
        Nothing
            | length (runningParticipants runningGame) >= humanSeatCount runningGame ->
                p_ "This lobby is full."
            | otherwise ->
                form_ [method_ "post", action_ (pathText ("/games/" ++ showGameId (runningGameId runningGame) ++ "/join")), class_ "control-stack"] $ do
                    label_ [class_ "control-label"] $ do
                        span_ "Your Name"
                        input_ [type_ "text", name_ "playerName", value_ "Player", maxlength_ "32"]
                    button_ [type_ "submit"] "Join Lobby"

submissionStatusPanel :: RunningGame -> Maybe HumanPlayer -> Maybe Int -> Html ()
submissionStatusPanel runningGame maybePlayer maybeSecondsRemaining = do
    p_ [class_ "status-line"] $ do
        strong_ (toHtml (show (length (filter (`playerSubmittedTurn` runningGame) (requiredSubmitters runningGame)))))
        " / "
        strong_ (toHtml (show (length (requiredSubmitters runningGame))))
        " ready this round."
    p_ [class_ "status-line"] (toHtml (timerStatusText runningGame maybeSecondsRemaining))
    case maybePlayer of
        Nothing -> p_ "Observer view."
        Just participant ->
            if playerCanSubmitTurn (humanPlayerId participant) runningGame
                then p_ "You have not submitted yet."
                else
                    if playerSubmittedTurn (humanPlayerId participant) runningGame
                        then p_ "You are locked in for this round."
                        else p_ "No action needed from your seat right now."

playerActionsPanel :: RunningGame -> HumanPlayer -> Html ()
playerActionsPanel runningGame participant =
    let playerId = humanPlayerId participant
        plan = getPlayerPlan playerId runningGame
        actionBase = scopedBasePath runningGame (Just participant)
     in div_ [class_ "actions-list"] $ do
            p_ [class_ "status-line"] $
                "Staged orders: "
                    <> strong_ (toHtml (show (length (planOrders plan))))
                    <> " | Tickets: "
                    <> strong_ (toHtml (show (planTicketCount plan)))
            if playerCanEditPlan playerId runningGame
                then do
                    form_
                        [ method_ "post"
                        , action_ (pathText (actionBase ++ "/orders"))
                        , class_ "actions-list"
                        , makeAttribute "hx-post" (pathText (actionBase ++ "/orders"))
                        , makeAttribute "hx-target" "#game-shell"
                        , makeAttribute "hx-swap" "outerHTML"
                        ]
                        $ div_ [class_ "inline-fieldset"]
                        $ do
                            label_ [class_ "control-label"] $ do
                                span_ "Side"
                                select_ [name_ "side"] $ do
                                    option_ [value_ "Buy"] "Buy"
                                    option_ [value_ "Sell"] "Sell"
                            label_ [class_ "control-label"] $ do
                                span_ "Asset"
                                select_ [name_ "assetId"] $ do
                                    option_ [value_ "TLN101"] "TLN101"
                                    option_ [value_ "TLN102"] "TLN102"
                                    option_ [value_ "TLN103"] "TLN103"
                            label_ [class_ "control-label"] $ do
                                span_ "Quantity"
                                input_ [type_ "number", name_ "quantity", min_ "1", value_ "1"]
                            label_ [class_ "control-label"] $ do
                                span_ "Price"
                                input_ [type_ "number", name_ "limitPrice", min_ "1", value_ "1"]
                            button_ [type_ "submit"] "Add Order"
                    form_
                        [ method_ "post"
                        , action_ (pathText (actionBase ++ "/tickets"))
                        , class_ "actions-list"
                        , makeAttribute "hx-post" (pathText (actionBase ++ "/tickets"))
                        , makeAttribute "hx-target" "#game-shell"
                        , makeAttribute "hx-swap" "outerHTML"
                        ]
                        $ div_ [class_ "inline-fieldset compact"]
                        $ do
                            label_ [class_ "control-label"] $ do
                                span_ (toHtml (lotteryInputLabel (runningState runningGame)))
                                input_ [type_ "number", name_ "ticketCount", min_ "0", value_ (Text.pack (show (planTicketCount plan)))]
                            button_ [type_ "submit"] "Set Tickets"
                else p_ "Your plan is locked for this round."
            stagedOrdersTable runningGame participant

playerInventoryPanel :: RunningGame -> HumanPlayer -> Html ()
playerInventoryPanel runningGame participant =
    case humanPlayerEntityId participant of
        Nothing -> p_ "No seat assigned yet."
        Just entityId' ->
            let ledger = Map.findWithDefault Map.empty entityId' (gameHoldings (runningState runningGame))
             in holdingsTable
                    [ (TLN001, Map.findWithDefault 0 (assetSeriesId TLN001) ledger)
                    , (TLN101, Map.findWithDefault 0 (assetSeriesId TLN101) ledger)
                    , (TLN102, Map.findWithDefault 0 (assetSeriesId TLN102) ledger)
                    , (TLN103, Map.findWithDefault 0 (assetSeriesId TLN103) ledger)
                    ]

stagedOrdersTable :: RunningGame -> HumanPlayer -> Html ()
stagedOrdersTable runningGame participant =
    let playerId = humanPlayerId participant
        orders = planOrders (getPlayerPlan playerId runningGame)
        actionBase = scopedBasePath runningGame (Just participant)
     in if null orders
            then p_ "No staged orders."
            else table_ [class_ "data-table"] $ do
                thead_ $
                    tr_ $ do
                        th_ "Side"
                        th_ "Asset"
                        th_ "Qty"
                        th_ "Price"
                        th_ ""
                tbody_ $
                    mapM_
                        ( \order ->
                            tr_ $ do
                                td_ (toHtml (show (orderSide order)))
                                td_ (toHtml (orderBaseAsset order))
                                td_ (toHtml (show (orderQuantity order)))
                                td_ (toHtml (show (orderLimitPrice order)))
                                td_ $
                                    if playerCanEditPlan playerId runningGame
                                        then
                                            form_
                                                [ method_ "post"
                                                , action_ (pathText (actionBase ++ "/orders/" ++ showOrderId (orderId order) ++ "/delete"))
                                                , class_ "inline-form"
                                                , makeAttribute "hx-post" (pathText (actionBase ++ "/orders/" ++ showOrderId (orderId order) ++ "/delete"))
                                                , makeAttribute "hx-target" "#game-shell"
                                                , makeAttribute "hx-swap" "outerHTML"
                                                ]
                                                $ button_ [type_ "submit", class_ "secondary-button"] "Remove"
                                        else mempty
                        )
                        orders

roundControls :: RunningGame -> Html ()
roundControls runningGame =
    let actionBase = "/games/" ++ showGameId (runningGameId runningGame)
        currentRound = gameRoundNumber (runningState runningGame)
     in div_ [class_ "control-stack"] $ do
            form_
                [ method_ "post"
                , action_ (pathText (actionBase ++ "/resolve"))
                , class_ "inline-form resolve-form"
                , makeAttribute "hx-post" (pathText (actionBase ++ "/resolve"))
                , makeAttribute "hx-target" "#game-shell"
                , makeAttribute "hx-swap" "outerHTML"
                ]
                $ button_ [type_ "submit"]
                $ do
                    span_ "Resolve Round"
                    span_ [class_ "resolve-indicator", makeAttribute "aria-hidden" "true"] "..."
            form_
                [ method_ "post"
                , action_ (pathText (actionBase ++ "/advance"))
                , class_ "control-row"
                , makeAttribute "hx-post" (pathText (actionBase ++ "/advance"))
                , makeAttribute "hx-target" "#game-shell"
                , makeAttribute "hx-swap" "outerHTML"
                ]
                $ do
                    label_ [class_ "control-label"] $ do
                        span_ "Advance N Rounds"
                        input_ [type_ "number", name_ "count", min_ "1", value_ "10"]
                    button_ [type_ "submit"] "Advance"
            form_
                [ method_ "post"
                , action_ (pathText (actionBase ++ "/advance-to-round"))
                , class_ "control-row"
                , makeAttribute "hx-post" (pathText (actionBase ++ "/advance-to-round"))
                , makeAttribute "hx-target" "#game-shell"
                , makeAttribute "hx-swap" "outerHTML"
                ]
                $ do
                    label_ [class_ "control-label"] $ do
                        span_ "Advance To Round"
                        input_ [type_ "number", name_ "targetRound", min_ "1", value_ (Text.pack (show (currentRound + 10)))]
                    button_ [type_ "submit"] "Go"
            form_
                [ method_ "post"
                , action_ (pathText (actionBase ++ "/advance-to-end"))
                , class_ "inline-form"
                , makeAttribute "hx-post" (pathText (actionBase ++ "/advance-to-end"))
                , makeAttribute "hx-target" "#game-shell"
                , makeAttribute "hx-swap" "outerHTML"
                ]
                $ button_ [type_ "submit"] "Advance To End"
            p_ [class_ "control-note"] "Debug only. Human rounds normally advance through player submissions."

holdingsTable :: [(AssetId, Quantity)] -> Html ()
holdingsTable rows =
    table_ [class_ "data-table"] $ do
        thead_ $
            tr_ $ do
                th_ "Asset"
                th_ "Quantity"
        tbody_ $
            mapM_
                ( \(asset, quantity) ->
                    tr_ $ do
                        td_ (toHtml (show asset))
                        td_ (toHtml (show quantity))
                )
                rows

overviewList :: RunningGame -> Html ()
overviewList runningGame =
    let state = runningState runningGame
     in ul_ [class_ "compact-list"] $ do
            li_ ("Alive mortals: " <> toHtml (show (length (livingMortals state))))
            li_ ("Winner: " <> winnerLabel (gameWinner state))
            li_ ("Humans: " <> toHtml (show (length (runningParticipants runningGame))) <> " / " <> toHtml (show (humanSeatCount runningGame)))
            li_ ("NPC seats: " <> toHtml (show (runningNpcCount runningGame)))
            li_ ("Round timer: " <> toHtml (timerLabel runningGame))
            li_ ("History entries: " <> toHtml (show (length (runningHistory runningGame))))

marketsPanel :: RunningGame -> Maybe HumanPlayer -> Html ()
marketsPanel runningGame maybePlayer =
    let state = runningState runningGame
        markets = Map.elems (gameMarkets state)
     in if null markets
            then p_ "No markets configured."
            else div_ [class_ "actions-list"] $ mapM_ (marketCard runningGame maybePlayer state) markets

marketCard :: RunningGame -> Maybe HumanPlayer -> GameState -> Market -> Html ()
marketCard runningGame maybePlayer state market =
    let ownerLabel =
            case Map.lookup (marketOwner market) (gameEntities state) of
                Nothing -> showEntityId (marketOwner market)
                Just entity -> entityName entity
        editable = canEditMarketRules maybePlayer market
     in div_ [class_ "market-card"] $ do
            p_ [class_ "status-line"] $ do
                strong_ (toHtml (marketName market))
                " | Owner: "
                toHtml ownerLabel
            p_ [class_ "control-note"] $
                "Pairs: " <> toHtml (showMarketPairs (marketPairs market))
            p_ [class_ "control-note"] $
                "Owner-issued currencies: " <> toHtml (showOwnerCurrencies state market)
            ul_ [class_ "compact-list"] $
                mapM_ (marketRuleSummary state market) allMarketRules
            if editable
                then marketRuleEditor runningGame maybePlayer market
                else p_ [class_ "control-note"] "This market's owner must edit its rules."

marketRuleEditor :: RunningGame -> Maybe HumanPlayer -> Market -> Html ()
marketRuleEditor runningGame maybePlayer market =
    let enabled = ruleEnabled QuoteAssetMustBeOwnerIssuedCurrency market
     in form_
            [ method_ "post"
            , action_ (pathText (scopedMarketRulesPath runningGame maybePlayer market))
            , class_ "actions-list"
            , makeAttribute "hx-post" (pathText (scopedMarketRulesPath runningGame maybePlayer market))
            , makeAttribute "hx-target" "#game-shell"
            , makeAttribute "hx-swap" "outerHTML"
            ]
            $ div_ [class_ "inline-fieldset compact"]
            $ do
                label_ [class_ "control-label"] $ do
                    span_ "Quote Asset Must Be Owner-Issued Currency"
                    select_ [name_ "enabled"] $ do
                        option_ (withSelected enabled [value_ "true"]) "On"
                        option_ (withSelected (not enabled) [value_ "false"]) "Off"
                button_ [type_ "submit"] "Update Rule"

marketRuleSummary :: GameState -> Market -> MarketRule -> Html ()
marketRuleSummary state market marketRule =
    li_ $
        if ruleEnabled marketRule market
            then "Enabled: " <> toHtml (marketRuleLabel state market marketRule)
            else "Disabled: " <> toHtml (marketRuleLabel state market marketRule)

marketRuleLabel :: GameState -> Market -> MarketRule -> String
marketRuleLabel state market marketRule =
    case marketRule of
        QuoteAssetMustBeOwnerIssuedCurrency ->
            "Trades must be quoted in an owner-issued currency (" ++ showOwnerCurrencies state market ++ ")."

showMarketPairs :: [(SeriesId, SeriesId)] -> String
showMarketPairs pairs =
    case pairs of
        [] -> "none"
        _ -> unwords (map showPair pairs)
  where
    showPair (baseAsset, quoteAsset) = baseAsset ++ "/" ++ quoteAsset

showOwnerCurrencies :: GameState -> Market -> String
showOwnerCurrencies state market =
    case ownerCurrencyAssets state market of
        [] -> "none"
        assets -> unwords (map show assets)

ownerCurrencyAssets :: GameState -> Market -> [AssetId]
ownerCurrencyAssets state market =
    [ assetId
    | (assetId, issuerId) <- Map.toList (gameAssetIssuers state)
    , issuerId == marketOwner market
    , isCurrencyAsset assetId
    ]

allMarketRules :: [MarketRule]
allMarketRules = [QuoteAssetMustBeOwnerIssuedCurrency]

ruleEnabled :: MarketRule -> Market -> Bool
ruleEnabled marketRule market = marketRule `elem` marketRules market

withSelected :: Bool -> [Attribute] -> [Attribute]
withSelected isSelected attrs =
    if isSelected
        then attrs ++ [makeAttribute "selected" "selected"]
        else attrs

canEditMarketRules :: Maybe HumanPlayer -> Market -> Bool
canEditMarketRules maybePlayer market =
    case maybePlayer of
        Nothing -> True
        Just participant ->
            case humanPlayerEntityId participant of
                Nothing -> False
                Just entityId' -> entityId' == marketOwner market

scopedMarketRulesPath :: RunningGame -> Maybe HumanPlayer -> Market -> String
scopedMarketRulesPath runningGame maybePlayer market =
    scopedBasePath runningGame maybePlayer
        ++ "/markets/"
        ++ showMarketId (marketId market)
        ++ "/rules"

renderReportSummary :: RoundReport -> Html ()
renderReportSummary report = do
    p_ ("Round " <> toHtml (show (reportRoundNumber report)))
    ul_ [class_ "compact-list"] $ do
        li_ ("Orders submitted: " <> toHtml (show (length (reportSubmittedOrders report))))
        li_ ("Invalid orders: " <> toHtml (show (length (reportInvalidOrders report))))
        li_ ("Fills: " <> toHtml (show (length (reportFills report))))
        li_ ("Expirations: " <> toHtml (show (length (reportExpiredOrders report))))
        li_ ("Lottery purchases: " <> toHtml (show (length (reportLotteryPurchases report))))
        li_ ("Lottery issuances: " <> toHtml (show (length (reportLotteryIssuances report))))
        li_ ("Lottery settlements: " <> toHtml (show (length (reportLotterySettlements report))))
        li_ ("Refund recipient: " <> toHtml (fromMaybe "none" (fmap showEntityId reportRefundRecipient')))
  where
    reportRefundRecipient' = reportRefundRecipient report

historyList :: [RoundReport] -> Html ()
historyList reports =
    if null reports
        then p_ "No round history yet."
        else
            ol_ [class_ "compact-list"] $
                mapM_
                    ( \report ->
                        li_ $
                            "Round "
                                <> toHtml (show (reportRoundNumber report))
                                <> ": "
                                <> toHtml (show (length (reportFills report)))
                                <> " fills, "
                                <> toHtml (show (length (reportLotterySettlements report)))
                                <> " lottery settlements"
                    )
                    (reverse reports)

renderLotteryResultsSummary :: RoundReport -> Html ()
renderLotteryResultsSummary report =
    if null (reportLotteryIssuances report) && null (reportLotterySettlements report)
        then p_ "No lottery activity last round."
        else do
            p_ ("Round " <> toHtml (show (reportRoundNumber report)))
            ul_ [class_ "compact-list"] $ do
                mapM_ renderLotteryIssuanceLine (reportLotteryIssuances report)
                mapM_ renderLotterySettlementLine (reportLotterySettlements report)

renderLotteryIssuanceLine :: LotteryIssuance -> Html ()
renderLotteryIssuanceLine issuance =
    li_ $
        toHtml (showEntityId (lotteryIssuanceEntityId issuance))
            <> ": issued "
            <> toHtml (show (lotteryIssuanceTicketCount issuance))
            <> " tickets of "
            <> toHtml (lotteryIssuanceSeriesId issuance)

renderLotterySettlementLine :: LotterySettlement -> Html ()
renderLotterySettlementLine settlement =
    li_ $
        toHtml (showEntityId (lotterySettlementEntityId settlement))
            <> ": settled "
            <> toHtml (show (lotterySettlementTicketCount settlement))
            <> " tickets of "
            <> toHtml (lotterySettlementSeriesId settlement)
            <> ", "
            <> toHtml (show (lotterySettlementWinCount settlement))
            <> " wins, payout "
            <> toHtml (show (lotterySettlementPayoutQuantity settlement))

lotteryMenuPanel :: GameState -> Html ()
lotteryMenuPanel state =
    if null (gameLotteryMenu state)
        then p_ "No lottery offers are available."
        else ul_ [class_ "compact-list"] $ mapM_ renderLotteryOfferLine (gameLotteryMenu state)

renderLotteryOfferLine :: LotteryOffer -> Html ()
renderLotteryOfferLine offer =
    li_ $
        toHtml (show (lotteryOfferAssetId offer))
            <> ": pay "
            <> toHtml (show (lotteryOfferTicketPrice offer))
            <> " TLN001 for "
            <> toHtml (show (lotteryOfferOddsNumerator offer))
            <> "/"
            <> toHtml (show (lotteryOfferOddsDenominator offer))
            <> " odds of "
            <> toHtml (show (lotteryOfferPayoutQuantity offer))
            <> " "
            <> toHtml (show (lotteryOfferAssetId offer))

lotteryInputLabel :: GameState -> String
lotteryInputLabel state =
    case gameLotteryMenu state of
        offer : _ ->
            "Tickets for "
                ++ show (lotteryOfferAssetId offer)
                ++ " ("
                ++ show (lotteryOfferTicketPrice offer)
                ++ " TLN001 each)"
        [] -> "Lottery Tickets"

winnerLabel :: Maybe EntityId -> Html ()
winnerLabel maybeWinner =
    case maybeWinner of
        Nothing -> "none"
        Just entityId' -> "winner " <> toHtml (showEntityId entityId')

winnerText :: Maybe EntityId -> String
winnerText maybeWinner =
    case maybeWinner of
        Nothing -> "none"
        Just entityId' -> "winner " ++ showEntityId entityId'

renderCurrentPlayerNote :: Maybe HumanPlayer -> Html ()
renderCurrentPlayerNote maybePlayer =
    case maybePlayer of
        Nothing -> mempty
        Just participant ->
            p_ [class_ "player-note"] $
                case humanPlayerEntityId participant of
                    Nothing -> "You are joined as " <> toHtml (humanPlayerName participant) <> "."
                    Just entityId' ->
                        "You are " <> toHtml (humanPlayerName participant) <> " (seat " <> toHtml (showEntityId entityId') <> ")."

renderPrimaryAction :: RunningGame -> Maybe HumanPlayer -> Html ()
renderPrimaryAction runningGame maybePlayer =
    case maybePlayer of
        Nothing ->
            form_
                [ method_ "post"
                , action_ (pathText ("/games/" ++ showGameId (runningGameId runningGame) ++ "/resolve"))
                , class_ "inline-form resolve-form"
                , makeAttribute "hx-post" (pathText ("/games/" ++ showGameId (runningGameId runningGame) ++ "/resolve"))
                , makeAttribute "hx-target" "#game-shell"
                , makeAttribute "hx-swap" "outerHTML"
                ]
                $ button_ [type_ "submit"]
                $ do
                    span_ "Resolve Round"
                    span_ [class_ "resolve-indicator", makeAttribute "aria-hidden" "true"] "..."
        Just participant ->
            if isJust (gameWinner (runningState runningGame))
                then button_ [type_ "button", class_ "secondary-button", makeAttribute "disabled" "disabled"] "Game Over"
                else
                    if playerSubmittedTurn (humanPlayerId participant) runningGame
                        then button_ [type_ "button", class_ "secondary-button", makeAttribute "disabled" "disabled"] "Submitted"
                        else
                            if playerCanSubmitTurn (humanPlayerId participant) runningGame
                                then form_
                                    [ method_ "post"
                                    , action_ (pathText (scopedBasePath runningGame maybePlayer ++ "/submit"))
                                    , class_ "inline-form resolve-form"
                                    , makeAttribute "hx-post" (pathText (scopedBasePath runningGame maybePlayer ++ "/submit"))
                                    , makeAttribute "hx-target" "#game-shell"
                                    , makeAttribute "hx-swap" "outerHTML"
                                    ]
                                    $ do
                                        input_ [type_ "hidden", name_ "expectedRound", value_ (Text.pack (show (gameRoundNumber (runningState runningGame))))]
                                        button_ [type_ "submit"] $ do
                                            span_ "Submit Turn"
                                            span_ [class_ "resolve-indicator", makeAttribute "aria-hidden" "true"] "..."
                                else button_ [type_ "button", class_ "secondary-button", makeAttribute "disabled" "disabled"] "Waiting"

roundPhaseSummary :: RunningGame -> Maybe Int -> String
roundPhaseSummary runningGame maybeSecondsRemaining =
    if isJust (gameWinner (runningState runningGame))
        then "Final state."
        else
            let submitted = length (filter (`playerSubmittedTurn` runningGame) (requiredSubmitters runningGame))
                required = length (requiredSubmitters runningGame)
                timerSummary = timerStatusText runningGame maybeSecondsRemaining
             in show submitted ++ "/" ++ show required ++ " players submitted. " ++ timerSummary

timerStatusText :: RunningGame -> Maybe Int -> String
timerStatusText runningGame maybeSecondsRemaining =
    case runningRoundTimeLimitSeconds runningGame of
        Nothing -> "Manual round progression."
        Just _ ->
            case maybeSecondsRemaining of
                Nothing -> "Timed rounds enabled."
                Just seconds -> "Auto-resolves in " ++ show seconds ++ "s if anyone is still missing."

playerRoundState :: HumanPlayer -> RunningGame -> String
playerRoundState participant runningGame =
    let playerId = humanPlayerId participant
     in if playerSubmittedTurn playerId runningGame
            then "submitted"
            else
                if playerCanSubmitTurn playerId runningGame
                    then "waiting"
                    else "idle"

timerLabel :: RunningGame -> String
timerLabel runningGame =
    case runningRoundTimeLimitSeconds runningGame of
        Nothing -> "manual"
        Just seconds -> show seconds ++ "s"

humanSeatSummary :: RunningGame -> String
humanSeatSummary runningGame =
    show (length (runningParticipants runningGame))
        ++ "/"
        ++ show (humanSeatCount runningGame)
        ++ npcSummarySuffix runningGame

npcSummarySuffix :: RunningGame -> String
npcSummarySuffix runningGame =
    if runningNpcCount runningGame <= 0
        then ""
        else
            " + "
                ++ show (runningNpcCount runningGame)
                ++ " NPC"
                ++ if runningNpcCount runningGame == 1 then "" else "s"

lobbyTimerLabel :: RunningGame -> String
lobbyTimerLabel runningGame =
    case runningRoundTimeLimitSeconds runningGame of
        Nothing -> "Manual rounds. Every live player must submit."
        Just seconds -> "Timed rounds at " ++ show seconds ++ "s. Missing players default to no action."

npcSeatName :: Int -> RunningGame -> String
npcSeatName seatIndex runningGame =
    case Map.lookup (EntityId seatIndex) (gameEntities (runningState runningGame)) of
        Just entity -> entityName entity <> " (NPC)"
        Nothing -> "Reserved NPC"

renderAgentationMount :: RunningGame -> Maybe HumanPlayer -> Html ()
renderAgentationMount _runningGame _maybePlayer = do
    div_
        [ id_ "agentation-root"
        , makeAttribute "data-endpoint" "http://localhost:4747"
        ]
        mempty
    with
        (script_ ("" :: Text.Text))
        [src_ "/static/agentation.bundle.js"]

scopedBasePath :: RunningGame -> Maybe HumanPlayer -> String
scopedBasePath runningGame maybePlayer =
    case maybePlayer of
        Nothing -> "/games/" ++ showGameId (runningGameId runningGame)
        Just participant ->
            "/games/"
                ++ showGameId (runningGameId runningGame)
                ++ "/players/"
                ++ showPlayerId (humanPlayerId participant)

pathText :: String -> Text.Text
pathText = Text.pack

showGameId :: GameId -> String
showGameId (GameId gameId) = show gameId

showMarketId :: MarketId -> String
showMarketId (MarketId marketIdValue) = show marketIdValue

showPlayerId :: PlayerId -> String
showPlayerId (PlayerId playerId) = show playerId

showOrderId :: OrderId -> String
showOrderId (OrderId orderIdValue) = show orderIdValue

showEntityId :: EntityId -> String
showEntityId (EntityId entityId) = show entityId
