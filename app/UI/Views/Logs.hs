module UI.Views.Logs where

import Brick.Types
import Brick.Main
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Logs as L

import Lens.Micro

draw :: AppState -> [Widget Name]
draw s = Main.draw s $ L.mkWidget (s^.logsStateL)

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do logsState' <- L.handleEvent ev (s^.logsStateL)
             continue $ s & logsStateL .~ logsState'
