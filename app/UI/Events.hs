module UI.Events where

import Data.Time.Clock

import Scylla.Types (ScyllaState)

data AppEvent =
    CIStateUpdate ScyllaState
  | Clock UTCTime
