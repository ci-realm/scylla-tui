
module UI.Types where

import TH (makeSuffixLenses)

import Control.Concurrent.STM (TChan)
import Data.Time

import Brick

import UI.Names
import UI.Widgets.Build
import UI.Widgets.Builds
import UI.Widgets.Orgs
import UI.Widgets.OrgBuilds
import UI.Widgets.Logs

import Scylla.Types (ScyllaState)
import qualified Scylla.Types.API as A

data View =
    LastBuilds
  | OrgBuilds
  | Orgs
  | Build
  | Logs
  deriving (Show, Eq, Ord)

logsScroll :: ViewportScroll Name
logsScroll = viewportScroll LogsVP

data AppState = AppState
  { _ciState :: ScyllaState
  , _time :: UTCTime
  , _buildState :: BuildState
  , _buildsState :: BuildsState
  , _orgsState :: OrgsState
  , _orgBuildsState :: OrgBuildsState
  , _logsState :: LogsState
  , _queryChan :: TChan (A.Query)
  , _showHelp :: Bool
  , _view :: View
  }

type NextState = EventM Name (Next AppState)

makeSuffixLenses ''AppState
