module UI.Views.OrgBuilds where

import Brick.Types
import Brick.Main
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Views.Main as Main
import qualified UI.Widgets.OrgBuilds as OB

import Lens.Micro

draw :: AppState -> [Widget Name]
draw s = Main.draw s $ OB.mkWidget (s^.orgBuildsStateL)

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do orgBuildsState' <- OB.handleEvent ev (s^.orgBuildsStateL)
             continue $ s & orgBuildsStateL .~ orgBuildsState'
