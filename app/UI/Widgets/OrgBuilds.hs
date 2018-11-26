module UI.Widgets.OrgBuilds where

import qualified Data.Vector as Vector

import Data.Time.LocalTime
import Brick hiding (attrName)
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import UI.Events (AppEvent)
import UI.Names
import TH (makeSuffixLenses)
import Lens.Micro --((^.))
import qualified Graphics.Vty as V

import qualified Scylla.Types.Org as O
import qualified Scylla.Pretty as P

import qualified UI.Widgets.Builds as BSW

data OrgBuildsState = OrgBuildsState
  { _org :: Maybe O.Org
  , _builds :: BSW.BuildsState
  }

makeSuffixLenses ''OrgBuildsState

mkState :: P.PPConf -> OrgBuildsState
mkState pp = OrgBuildsState
  { _org = Nothing
  , _builds = BSW.mkState pp }

mkWidget :: OrgBuildsState -> Widget Name
mkWidget s = ui
    where
        box = border $ maybe (str "") drawOrg $ s^.orgL
        ui = vBox [ vCenter $ box , BSW.mkWidget (s^.buildsL) ]

drawOrg :: O.Org -> Widget Name
drawOrg O.Org{..} =
    hBox [ padLeft Max $ str owner
         , str " "
         , padLeft Max $ str url
         , str " "
         , padLeft Max $ str $ show buildCount
         ]

attrName :: AttrName
attrName = "org"

ownerAttr :: AttrName
ownerAttr = attrName <> "owner"

selectedAttr :: AttrName
selectedAttr = listSelectedAttr <> "selected"

attrs :: [(AttrName, V.Attr)]
attrs = [ (attrName, fg V.green)
        , (ownerAttr, fg V.green `V.withStyle` V.bold)
        , (selectedAttr, fg V.cyan `V.withStyle` V.bold)
        ]

handleEvent :: BrickEvent Name AppEvent -> OrgBuildsState -> EventM Name OrgBuildsState
handleEvent e@(VtyEvent ev) s = do
  blS <- BSW.handleEvent e (s^.buildsL)
  return $ s & buildsL .~ blS
