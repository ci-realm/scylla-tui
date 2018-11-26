module UI.Views.Main where

import Data.Time.Format

import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Types
import Brick.Main
import qualified Graphics.Vty as V
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Widgets.Builds as B
import Lens.Micro

draw :: AppState -> Widget Name -> [Widget Name]
draw s widget = [ui]
  where
    ui = vCenter $ vBox widgets
    activeViewWidget = str $ show (s^.viewL)
    view = str "       " <+> hCenter activeViewWidget <+> str "?: Help"
    tabs view = (mkTab 1 LastBuilds view "Last builds") <+> (mkTab 2 Orgs view "Organizations")
    mkTab n forView view pretty =
      let active = if forView == view
                   then withAttr selectedTabAttr
                   else id
      in active (str "[" <+> str (show n) <+> str "] ") <+> withAttr tabNameAttr (str pretty) <+> str " "

    widgets = [
        (tabs $ s^.viewL) <+> (padLeft Max $ str $ formatTime defaultTimeLocale "%F %X" $ s^.timeL)
--      , hBorder
      , hCenter widget
      , view
      ]

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do
      case s^.viewL of
        LastBuilds -> do buildsState' <- B.handleEvent ev (s^.buildsStateL)
                         continue $ s & buildsStateL .~ buildsState' 

selectedTabAttr :: AttrName
selectedTabAttr = "selectedTab"

tabNameAttr :: AttrName
tabNameAttr = "tabName"

attrs :: [(AttrName, V.Attr)]
attrs = [ (tabNameAttr, fg V.yellow)
        , (selectedTabAttr, fg V.white `V.withStyle` V.bold)
        ]

