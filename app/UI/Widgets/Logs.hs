module UI.Widgets.Logs where

import qualified Data.Vector as Vector

import Data.Time.LocalTime
import Data.Time.Format
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
import qualified Scylla.Types.ShortLog as S

import qualified Scylla.Pretty as P
import UI.Widgets.Build (statusAttr)

import Text.Wrap (defaultWrapSettings, preserveIndentation)

data LogsState = LogsState
  { _loglines :: [S.ShortLog]
  , _build :: Maybe B.Build
  , _ppConf :: P.PPConf
  }

makeSuffixLenses ''LogsState

mkState :: P.PPConf -> LogsState
mkState pp = LogsState
  { _loglines = []
  , _build = Nothing
  , _ppConf = pp }

mkWidget :: LogsState -> Widget Name
mkWidget s = ui
    where
        label = str "Logs for build " <+> str (maybe "" (show . B.id) (s^.buildL))
        box = borderWithLabel label $
              viewport LogsVP Vertical $ vBox $ map go $ s^.loglinesL
        go S.ShortLog{..} = str (formatTime defaultTimeLocale "%F %X" time)
          <+> str ":"
          <+> strWrapWith settings line

        ui = vCenter $ box
        settings = defaultWrapSettings
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

handleEvent :: BrickEvent Name AppEvent -> LogsState -> EventM Name LogsState
handleEvent e@(VtyEvent ev) s = return s
