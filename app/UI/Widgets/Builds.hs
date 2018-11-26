module UI.Widgets.Builds where

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

import qualified Scylla.Types.Build as B
import qualified Scylla.Types.BuildStatus as BS

import qualified Scylla.Pretty as P
import UI.Widgets.Build (statusAttr)

data BuildsState = BuildsState
  { _list :: List Name B.Build
  , _ppConf :: P.PPConf
  }

makeSuffixLenses ''BuildsState

mkState :: P.PPConf -> BuildsState
mkState pp = BuildsState
  { _list = list BuildList (Vector.empty) 1
  , _ppConf = pp }

mkWidget :: BuildsState -> Widget Name
mkWidget s = ui
    where
        label = str "Build " <+> cur <+> str " of " <+> total
        cur = case s^.listL.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vector.length $ s^.listL.listElementsL
        box = borderWithLabel label $
              renderList (drawBuild timeFn) True $ s^.listL
        ui = vCenter $ box
        timeFn = P.timeRenderer $ s^.ppConfL

drawBuild :: (ZonedTime -> String) -> Bool -> B.Build -> Widget Name
drawBuild timeFn sel B.Build{..} =
    let selStr s = if sel
                   then withAttr selectedAttr (str $ ">" <> s)
                   else str s
    in hBox [ hLimit 6 $ padLeft Max $ withAttr (statusAttr status) $ selStr $ show id
            , str " "
            , hLimit 9 $ padRight Max $ withAttr (statusAttr status) $ str $ show status
            , hLimit 14 $ padLeft Max $ withAttr timeAgoAttr $ str $ timeFn createdAt
            , str " "
            , padRight Max $ withAttr projectNameAttr $ str projectName
            , str $ P.ppFinished status finishedAt createdAt ]


attrName :: AttrName
attrName = "build"

projectNameAttr :: AttrName
projectNameAttr = attrName <> "projectName"

timeAgoAttr :: AttrName
timeAgoAttr = attrName <> "timeAgo"

selectedAttr :: AttrName
selectedAttr = listSelectedAttr <> "selected"

attrs :: [(AttrName, V.Attr)]
attrs = [ (attrName, fg V.green)
        , (projectNameAttr, fg V.green `V.withStyle` V.bold)
        , (timeAgoAttr, fg V.yellow)
        , (selectedAttr, fg V.cyan `V.withStyle` V.bold)
        ]

handleEvent :: BrickEvent Name AppEvent -> BuildsState -> EventM Name BuildsState
handleEvent e@(VtyEvent ev) s = case e of
  _ -> do
    l' <- handleListEvent ev (s^.listL)
    return $ s & listL .~ l'
