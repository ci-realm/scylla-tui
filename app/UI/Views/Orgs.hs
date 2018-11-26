module UI.Views.Orgs where

import Brick.Types
import Brick.Main
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Orgs as O

import Lens.Micro

draw :: AppState -> [Widget Name]
draw s = Main.draw s $ O.mkWidget (s^.orgsStateL)

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do orgsState' <- O.handleEvent ev (s^.orgsStateL)
             continue $ s & orgsStateL .~ orgsState'
