{-# LANGUAGE OverloadedStrings #-}

module Tlon.Web.View
  ( renderGamePage,
    renderGameShell,
    renderIndexPage,
    renderNotFoundPage,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Lucid
import Lucid.Base (makeAttribute)
import qualified Data.Text as Text
import Tlon.Core.Event
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Web.State

renderIndexPage :: [RunningGame] -> Html ()
renderIndexPage games =
  layout "Tlon" $ do
    section_ [class_ "band hero"] $ do
      div_ [class_ "wrap"] $ do
        h1_ "Tlon"
        p_ [class_ "lede"] "Obligatory lottery markets."
        form_ [method_ "post", action_ "/games", class_ "inline-form"] $
          button_ [type_ "submit"] "New Game"
    section_ [class_ "band"] $ do
      div_ [class_ "wrap"] $ do
        h2_ "Games"
        if null games
          then p_ "No games yet."
          else table_ [class_ "data-table"] $ do
            thead_ $
              tr_ $ do
                th_ "Game"
                th_ "Round"
                th_ "Status"
                th_ "Alive"
                th_ ""
            tbody_ $
              mapM_ renderGameRow games

renderGamePage :: RunningGame -> Html ()
renderGamePage runningGame =
  let gameLabel = showGameId (runningGameId runningGame)
   in
    layout ("Tlon Game " <> Text.pack gameLabel) $ do
      renderGameShell runningGame
      renderAgentationMount runningGame

renderGameShell :: RunningGame -> Html ()
renderGameShell runningGame =
  let state = runningState runningGame
      report = gamePreviousReport state
      gameLabel = showGameId (runningGameId runningGame)
   in
    div_
      [ id_ "game-shell",
        makeAttribute "data-game-id" (Text.pack gameLabel),
        makeAttribute "data-round-number" (Text.pack (show (gameRoundNumber state))),
        makeAttribute "data-has-winner" (if gameWinner state == Nothing then "false" else "true")
      ]
      $ do
      section_ [class_ "band hero"] $ do
        div_ [class_ "wrap"] $ do
          div_ [class_ "topbar"] $ do
            div_ $ do
              h1_ ("Game " <> toHtml gameLabel)
              p_ [class_ "lede"] $
                "Round " <> toHtml (show (gameRoundNumber state))
                  <> " | "
                  <> winnerLabel (gameWinner state)
            div_ [class_ "actions"] $ do
              a_ [href_ "/", class_ "button secondary"] "Games"
              form_
                [ method_ "post",
                  action_ (Text.pack ("/games/" ++ showGameId (runningGameId runningGame) ++ "/resolve")),
                  class_ "inline-form resolve-form",
                  makeAttribute "hx-post" (Text.pack ("/games/" ++ showGameId (runningGameId runningGame) ++ "/resolve")),
                  makeAttribute "hx-target" "#game-shell",
                  makeAttribute "hx-swap" "outerHTML"
                ]
                $ button_ [type_ "submit"] $ do
                  span_ "Resolve Round"
                  span_ [class_ "resolve-indicator", makeAttribute "aria-hidden" "true"] "..."
              form_
                [ method_ "post",
                  action_ (Text.pack ("/games/" ++ showGameId (runningGameId runningGame) ++ "/reset")),
                  class_ "inline-form reset-form",
                  makeAttribute "hx-post" (Text.pack ("/games/" ++ showGameId (runningGameId runningGame) ++ "/reset")),
                  makeAttribute "hx-target" "#game-shell",
                  makeAttribute "hx-swap" "outerHTML",
                  makeAttribute "hx-confirm" "Reset this game?"
                ]
                $ button_ [type_ "submit", class_ "secondary-button"] "Reset Game"
      section_ [class_ "band"] $ do
        div_ [class_ "wrap grid"] $ do
          div_ [class_ "panel"] $ do
            h2_ "Redemption"
            holdingsTable (Map.toList (gameRedemptionTable state))
          div_ [class_ "panel"] $ do
            h2_ "Overview"
            overviewList runningGame
          div_ [class_ "panel"] $ do
            h2_ "Round Controls"
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
        div_ [class_ "wrap grid"] $ do
          div_ [class_ "panel"] $ do
            h2_ "Latest Report"
            maybe (p_ "No rounds resolved yet.") renderReportSummary report
          div_ [class_ "panel"] $ do
            h2_ "History"
            historyList (runningHistory runningGame)

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
        (script_ autoplayScript)
        [type_ "text/javascript"]
      with
        (script_ ("" :: Text.Text))
        [src_ "/static/htmx.min.js"]
      style_ css
    body_ bodyContent

css :: Text.Text
css =
  Text.unlines
    [ "html { font-family: Inter, system-ui, sans-serif; background: #f4f1eb; color: #141414; }",
      "body { margin: 0; }",
      ".band { padding: 24px 0; border-top: 1px solid #d9d2c7; }",
      ".hero { background: #efe6d2; border-top: 0; }",
      ".wrap { max-width: 1120px; margin: 0 auto; padding: 0 20px; }",
      ".grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); gap: 16px; }",
      ".panel { background: #fbfaf7; border: 1px solid #d9d2c7; border-radius: 8px; padding: 16px; }",
      ".topbar { display: flex; justify-content: space-between; gap: 16px; align-items: end; flex-wrap: wrap; }",
      ".actions { display: flex; gap: 12px; align-items: center; flex-wrap: wrap; }",
      ".control-stack { display: grid; gap: 12px; }",
      ".control-row { display: grid; grid-template-columns: minmax(0, 1fr) auto; gap: 10px; align-items: end; }",
      ".control-row.triple { grid-template-columns: minmax(0, 1fr) auto auto; }",
      ".control-label { display: grid; gap: 6px; color: #50483d; }",
      ".control-label span { font-size: 14px; }",
      ".control-label input { width: 100%; box-sizing: border-box; border: 1px solid #cdbfae; border-radius: 6px; padding: 10px 12px; font: inherit; background: #fff; }",
      ".control-note { margin: -4px 0 0; font-size: 14px; color: #6a6155; }",
      ".lede { margin: 8px 0 0; color: #50483d; }",
      ".button, button { display: inline-block; background: #1f6f5f; color: #fff; border: 0; border-radius: 6px; padding: 10px 14px; font: inherit; text-decoration: none; cursor: pointer; }",
      ".button.secondary { background: #45332b; }",
      ".secondary-button { background: #7a3f2a; color: #fff; }",
      ".inline-form { margin: 0; }",
      ".resolve-form button { display: inline-flex; gap: 8px; align-items: center; }",
      ".resolve-form.htmx-request button { opacity: 0.72; }",
      ".resolve-indicator { display: none; }",
      ".resolve-form.htmx-request .resolve-indicator { display: inline; }",
      "h1, h2, h3 { margin: 0 0 12px; }",
      "p, ul { margin: 0 0 12px; }",
      ".data-table { width: 100%; border-collapse: collapse; background: #fbfaf7; border: 1px solid #d9d2c7; border-radius: 8px; overflow: hidden; }",
      ".data-table th, .data-table td { text-align: left; padding: 10px 12px; border-bottom: 1px solid #e6e0d6; vertical-align: top; }",
      ".data-table thead { background: #e6ded0; }",
      ".data-table tr:last-child td { border-bottom: 0; }",
      ".tag { display: inline-block; padding: 2px 8px; border-radius: 999px; background: #ddd3c6; }",
      ".tag.dead { background: #3f2d28; color: #fff; }",
      ".compact-list { padding-left: 18px; }",
      ".compact-list li { margin-bottom: 6px; }"
    ]

reloadScript :: Text.Text
reloadScript =
  Text.unlines
    [ "(function () {",
      "  var currentToken = null;",
      "  var started = false;",
      "  function poll() {",
      "    fetch('/dev/reload-token', { cache: 'no-store' })",
      "      .then(function (response) { return response.ok ? response.text() : null; })",
      "      .then(function (token) {",
      "        if (!token) return;",
      "        if (!started) {",
      "          currentToken = token;",
      "          started = true;",
      "          return;",
      "        }",
      "        if (currentToken !== token) {",
      "          window.location.reload();",
      "        }",
      "      })",
      "      .catch(function () {})",
      "      .finally(function () { window.setTimeout(poll, 1000); });",
      "  }",
      "  window.setTimeout(poll, 1000);",
      "})();"
    ]

autoplayScript :: Text.Text
autoplayScript =
  Text.unlines
    [ "(function () {",
      "  var autoplay = { running: false, timerId: null, delayMs: 1000 };",
      "  function getShell() { return document.getElementById('game-shell'); }",
      "  function stopAutoplay() {",
      "    autoplay.running = false;",
      "    if (autoplay.timerId !== null) { window.clearTimeout(autoplay.timerId); }",
      "    autoplay.timerId = null;",
      "  }",
      "  function scheduleNextTick() {",
      "    if (!autoplay.running) { return; }",
      "    autoplay.timerId = window.setTimeout(tick, autoplay.delayMs);",
      "  }",
      "  function replaceShell(html) {",
      "    var shell = getShell();",
      "    if (shell) { shell.outerHTML = html; }",
      "  }",
      "  function tick() {",
      "    var shell = getShell();",
      "    if (!shell || shell.dataset.hasWinner === 'true') { stopAutoplay(); return; }",
      "    var gameId = shell.dataset.gameId;",
      "    if (!gameId) { stopAutoplay(); return; }",
      "    fetch('/games/' + gameId + '/resolve', {",
      "      method: 'POST',",
      "      headers: { 'HX-Request': 'true' }",
      "    })",
      "      .then(function (response) { return response.ok ? response.text() : null; })",
      "      .then(function (html) { if (html) { replaceShell(html); } })",
      "      .catch(function () { stopAutoplay(); })",
      "      .finally(function () { scheduleNextTick(); });",
      "  }",
      "  document.addEventListener('click', function (event) {",
      "    var startButton = event.target.closest('[data-action=\"autoplay-start\"]');",
      "    if (startButton) {",
      "      event.preventDefault();",
      "      var secondsInput = document.getElementById('autoplay-seconds');",
      "      var seconds = secondsInput ? Number(secondsInput.value) : 1;",
      "      if (!Number.isFinite(seconds) || seconds <= 0) { seconds = 1; }",
      "      autoplay.delayMs = Math.max(100, seconds * 1000);",
      "      stopAutoplay();",
      "      autoplay.running = true;",
      "      tick();",
      "      return;",
      "    }",
      "    var stopButton = event.target.closest('[data-action=\"autoplay-stop\"]');",
      "    if (stopButton) {",
      "      event.preventDefault();",
      "      stopAutoplay();",
      "    }",
      "  });",
      "  document.addEventListener('htmx:beforeRequest', function (event) {",
      "    var shell = getShell();",
      "    if (shell && shell.dataset.hasWinner === 'true') { stopAutoplay(); }",
      "  });",
      "})();"
    ]

renderGameRow :: RunningGame -> Html ()
renderGameRow runningGame =
  let state = runningState runningGame
   in tr_ $ do
        td_ ("Game " <> toHtml (showGameId (runningGameId runningGame)))
        td_ (toHtml (show (gameRoundNumber state)))
        td_ (winnerLabel (gameWinner state))
        td_ (toHtml (show (length (livingMortals state))))
        td_ $
          a_ [href_ (Text.pack ("/games/" ++ showGameId (runningGameId runningGame))), class_ "button secondary"] "Open"

renderEntityRow :: GameState -> Entity -> Html ()
renderEntityRow state entity =
  let entityLedger = Map.findWithDefault Map.empty (entityId entity) (gameHoldings state)
      renderAsset :: AssetId -> Html ()
      renderAsset asset = td_ (toHtml (show (Map.findWithDefault 0 asset entityLedger)))
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

holdingsTable :: [(AssetId, Quantity)] -> Html ()
holdingsTable rows =
  table_ [class_ "data-table"] $ do
    thead_ $
      tr_ $ do
        th_ "Asset"
        th_ "Quantity"
    tbody_ $
      mapM_
        (\(asset, quantity) ->
           tr_ $ do
             td_ (toHtml (show asset))
             td_ (toHtml (show quantity))
        )
        rows

overviewList :: RunningGame -> Html ()
overviewList runningGame =
  let state = runningState runningGame
      totalGames = show (length (runningHistory runningGame))
   in ul_ [class_ "compact-list"] $ do
        li_ ("Alive mortals: " <> toHtml (show (length (livingMortals state))))
        li_ ("Winner: " <> winnerLabel (gameWinner state))
        li_ ("Resolved rounds: " <> toHtml totalGames)
        li_ ("History entries: " <> toHtml (show (length (runningHistory runningGame))))

roundControls :: RunningGame -> Html ()
roundControls runningGame =
  let gameId = showGameId (runningGameId runningGame)
      currentRound = gameRoundNumber (runningState runningGame)
   in div_ [class_ "control-stack"] $ do
        form_
          [ method_ "post",
            action_ (Text.pack ("/games/" ++ gameId ++ "/advance")),
            class_ "control-row",
            makeAttribute "hx-post" (Text.pack ("/games/" ++ gameId ++ "/advance")),
            makeAttribute "hx-target" "#game-shell",
            makeAttribute "hx-swap" "outerHTML"
          ]
          $ do
            label_ [class_ "control-label"] $ do
              span_ "Advance N Rounds"
              input_ [type_ "number", name_ "count", min_ "1", value_ "10"]
            button_ [type_ "submit"] "Advance"
        form_
          [ method_ "post",
            action_ (Text.pack ("/games/" ++ gameId ++ "/advance-to-round")),
            class_ "control-row",
            makeAttribute "hx-post" (Text.pack ("/games/" ++ gameId ++ "/advance-to-round")),
            makeAttribute "hx-target" "#game-shell",
            makeAttribute "hx-swap" "outerHTML"
          ]
          $ do
            label_ [class_ "control-label"] $ do
              span_ "Advance To Round"
              input_ [type_ "number", name_ "targetRound", min_ "1", value_ (Text.pack (show (currentRound + 10)))]
            button_ [type_ "submit"] "Go"
        form_
          [ method_ "post",
            action_ (Text.pack ("/games/" ++ gameId ++ "/advance-to-end")),
            class_ "inline-form",
            makeAttribute "hx-post" (Text.pack ("/games/" ++ gameId ++ "/advance-to-end")),
            makeAttribute "hx-target" "#game-shell",
            makeAttribute "hx-swap" "outerHTML"
          ]
          $ button_ [type_ "submit"] "Advance To End"
        p_ [class_ "control-note"] "Runs until a winner appears or the safety cap is reached."
        div_ [class_ "control-row triple"] $ do
          label_ [class_ "control-label"] $ do
            span_ "Seconds Per Round"
            input_ [type_ "number", id_ "autoplay-seconds", min_ "0.1", step_ "0.1", value_ "1.0"]
          button_ [type_ "button", makeAttribute "data-action" "autoplay-start"] "Start"
          button_ [type_ "button", class_ "secondary-button", makeAttribute "data-action" "autoplay-stop"] "Stop"

renderReportSummary :: RoundReport -> Html ()
renderReportSummary report = do
  p_ ("Round " <> toHtml (show (reportRoundNumber report)))
  ul_ [class_ "compact-list"] $ do
    li_ ("Orders submitted: " <> toHtml (show (length (reportSubmittedOrders report))))
    li_ ("Invalid orders: " <> toHtml (show (length (reportInvalidOrders report))))
    li_ ("Fills: " <> toHtml (show (length (reportFills report))))
    li_ ("Expirations: " <> toHtml (show (length (reportExpiredOrders report))))
    li_ ("Redemptions: " <> toHtml (show (length (reportRedemptions report))))
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
          (\report ->
             li_ $
               "Round "
                 <> toHtml (show (reportRoundNumber report))
                 <> ": "
                 <> toHtml (show (length (reportFills report)))
                 <> " fills, "
                 <> toHtml (show (length (reportRedemptions report)))
                 <> " redemptions"
          )
          (reverse reports)

winnerLabel :: Maybe EntityId -> Html ()
winnerLabel maybeWinner =
  case maybeWinner of
    Nothing -> "none"
    Just entityId' -> "winner " <> toHtml (showEntityId entityId')

renderAgentationMount :: RunningGame -> Html ()
renderAgentationMount runningGame = do
  div_
    [ id_ "agentation-root",
      makeAttribute "data-endpoint" "http://localhost:4747",
      makeAttribute "data-session-id" (Text.pack ("tlon-game-" ++ showGameId (runningGameId runningGame)))
    ]
    mempty
  with
    (script_ ("" :: Text.Text))
    [src_ "/static/agentation.bundle.js"]

showGameId :: GameId -> String
showGameId (GameId gameId) = show gameId

showEntityId :: EntityId -> String
showEntityId (EntityId entityId) = show entityId
