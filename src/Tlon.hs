module Tlon (
    module Tlon.Core.Engine,
    module Tlon.Core.Event,
    module Tlon.Core.OrderBook,
    module Tlon.Core.Rules,
    module Tlon.Core.State,
    module Tlon.Core.Types,
    module Tlon.Game.Default.Config,
    module Tlon.Game.Default.Setup,
    module Tlon.Game.Default.View,
    module Tlon.Web.Api,
    module Tlon.Web.Server,
    module Tlon.Web.State,
    module Tlon.Web.View,
    defaultGameState,
)
where

import Tlon.Core.Engine
import Tlon.Core.Event
import Tlon.Core.OrderBook
import Tlon.Core.Rules
import Tlon.Core.State
import Tlon.Core.Types
import Tlon.Game.Default.Config
import Tlon.Game.Default.Setup
import Tlon.Game.Default.View
import Tlon.Web.Api
import Tlon.Web.Server
import Tlon.Web.State
import Tlon.Web.View

defaultGameState :: GameState
defaultGameState = initialState defaultConfig
