module UI.Views.Builds where

import Brick.Types
import Brick.Main
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Builds as B

import Lens.Micro

draw :: AppState -> [Widget Name]
draw s = Main.draw s $ B.mkWidget (s^.buildsStateL)

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do buildsState' <- B.handleEvent ev (s^.buildsStateL)
             continue $ s & buildsStateL .~ buildsState'
