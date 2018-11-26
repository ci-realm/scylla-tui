module UI.Views.Build where

import Brick.Types
import Brick.Main
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Build as B

import Lens.Micro

draw :: AppState -> [Widget Name]
draw s = Main.draw s $ B.mkWidget (s^.buildStateL)

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do buildState' <- B.handleEvent ev (s^.buildStateL)
             continue $ s & buildStateL .~ buildState'
