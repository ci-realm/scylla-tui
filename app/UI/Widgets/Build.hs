module UI.Widgets.Build where

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

import qualified Scylla.Types.Log as L
import qualified Scylla.Types.Build as B
import qualified Scylla.Types.BuildStatus as BS

import qualified Scylla.Pretty as P

import Text.Wrap (defaultWrapSettings, preserveIndentation)
import Data.List.Split

data BuildState = BuildState
  { _build :: Maybe B.Build
  , _ppConf :: P.PPConf
  }

makeSuffixLenses ''BuildState

mkState :: Maybe B.Build -> P.PPConf -> BuildState
mkState b pp = BuildState
  { _build = b
  , _ppConf = pp }

mkWidget :: BuildState -> Widget Name
mkWidget s = maybe (str "Fetching build data") (drawBuild timeFn) (s^.buildL)
    where
        timeFn = P.timeRenderer $ s^.ppConfL

drawBuild :: (ZonedTime -> String) -> B.Build -> Widget Name
drawBuild timeFn B.Build{..} = vBox [
      borderWithLabel (str "Build #" <+> str (show id)) $ hBox
          [ hLimit 6 $ padLeft Max $ withAttr (statusAttr status) $ str $ show id
          , str " "
          , hLimit 9 $ padRight Max $ withAttr (statusAttr status) $ str $ show status
          , hLimit 14 $ padLeft Max $ withAttr timeAgoAttr $ str $ timeFn createdAt
          , str " "
          , padRight Max $ withAttr projectNameAttr $ str projectName
          , str $ P.ppFinished status finishedAt createdAt ]
    , hBorder
    , drawLog log  ]

drawLog log = viewport LogsVP Vertical $ vBox $ go log
  where go Nothing = [ str "No logs" ]
        go (Just logs) = concatMap (\L.Log{..} ->
          map (strWrapWith settings . drop 1 . dropWhile (/=' ')) (splitOn "\\n" line)) logs
        settings = defaultWrapSettings

statusAttr :: BS.BuildStatus -> AttrName
statusAttr BS.Success = successAttr
statusAttr BS.Failure = failureAttr
statusAttr BS.Build = buildAttr
statusAttr BS.Queue = queueAttr
statusAttr BS.Clone = cloneAttr
statusAttr BS.Checkout = checkoutAttr

attrName :: AttrName
attrName = "status"

projectNameAttr :: AttrName
projectNameAttr = "projectName"

timeAgoAttr :: AttrName
timeAgoAttr = "timeAgo"

successAttr :: AttrName
successAttr = attrName <> "success"

failureAttr :: AttrName
failureAttr = attrName <> "failure"

buildAttr :: AttrName
buildAttr = attrName <> "build"

queueAttr :: AttrName
queueAttr = attrName <> "queue"

cloneAttr :: AttrName
cloneAttr = attrName <> "clone"

checkoutAttr :: AttrName
checkoutAttr = attrName <> "checkout"

selectedAttr :: AttrName
selectedAttr = listSelectedAttr <> "selected"

attrs :: [(AttrName, V.Attr)]
attrs = [ (attrName, fg V.green)
        , (projectNameAttr, fg V.green `V.withStyle` V.bold)
        , (timeAgoAttr, fg V.yellow)
        , (successAttr, fg V.green)
        , (failureAttr, fg V.red)
        , (buildAttr, fg V.yellow)
        , (queueAttr, fg V.blue)
        , (cloneAttr, fg V.white)
        , (checkoutAttr, fg V.cyan)
        , (selectedAttr, fg V.cyan `V.withStyle` V.bold)
        ]

handleEvent :: BrickEvent Name AppEvent -> BuildState -> EventM Name BuildState
handleEvent e@(VtyEvent ev) s = return s
