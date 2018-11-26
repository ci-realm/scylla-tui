module UI.Widgets.Orgs where

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

data OrgsState = OrgsState
  { _list :: List Name O.Org
  }

makeSuffixLenses ''OrgsState

mkState :: OrgsState
mkState = OrgsState
  { _list = list OrgList Vector.empty 1 }

mkWidget :: OrgsState -> Widget Name
mkWidget s = ui
    where
        label = str "Organization " <+> cur <+> str " of " <+> total
        cur = case s^.listL.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vector.length $ s^.listL.listElementsL
        box = borderWithLabel label $
              renderList drawOrg True $ s^.listL
        ui = vCenter $ box

drawOrg :: Bool -> O.Org -> Widget Name
drawOrg sel O.Org{..} =
    let selStr s = if sel
                   then withAttr selectedAttr (str $ ">" <> s)
                   else str s
    in hBox [ padLeft Max $ selStr $ owner
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

handleEvent :: BrickEvent Name AppEvent -> OrgsState -> EventM Name OrgsState
handleEvent e@(VtyEvent ev) s = case e of
  _ -> do
    l' <- handleListEvent ev (s^.listL)
    return $ s & listL .~ l'
