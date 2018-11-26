{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Data.Vector as Vector

import Lens.Micro ((^.))
import Control.Monad
import Control.Monad.IO.Class
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Brick.BChan
import Brick.Types
import Brick.Forms
import Brick.Main
import qualified Brick.Widgets.Border as B
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , hBox
  , withAttr
  , padRight
  )
import Brick.Util (fg, on)

import Data.Default
import Data.Time
import Control.Monad
import Lens.Micro

import UI.Types
import UI.Events
import UI.Names
import qualified UI.Widgets.Build as BW
import qualified UI.Widgets.Builds as BSW
import qualified UI.Widgets.Orgs as OSW
import qualified UI.Widgets.OrgBuilds as OBW
import qualified UI.Widgets.Logs as LW
--import qualified UI.Widgets.ItemEdit as IWE

import qualified UI.Views.Build as BV
import qualified UI.Views.Builds as LBV
import qualified UI.Views.Orgs as OV
import qualified UI.Views.OrgBuilds as OBV
import qualified UI.Views.Logs as LV
import qualified UI.Views.Main as MV

import Scylla.Types (ScyllaState(..))
import qualified Scylla.Types.API as A
import qualified Scylla.Types.Build as B
import qualified Scylla.Types.Org as O
import Scylla.Client
import qualified Scylla.Pretty as P

drawUI :: AppState -> [Widget Name]
drawUI s = case s^.showHelpL of
  True -> [renderDialog (dialog (Just "Help") Nothing 50) drawHelp]
  False ->
    case s^.viewL of
      LastBuilds -> LBV.draw s
      Orgs -> OV.draw s
      OrgBuilds -> OBV.draw s
      Build -> BV.draw s
      Logs -> LV.draw s

drawHelp :: Widget n
drawHelp = vCenter $ vBox [ hCenter $ str "Help"
                          , hCenter $ str "Press 'q' or 'Esc' to exit."
                          , hCenter $ str "Press 'h' to toggle help."
                          , hCenter $ str "Ctrl-R to restart a build."
                          ]

handleViewEvent :: AppState -> BrickEvent Name AppEvent -> NextState
handleViewEvent s e = case s^.viewL of
  LastBuilds -> LBV.event s e
  Build -> BV.event s e
  Orgs -> OV.event s e
  OrgBuilds -> OBV.event s e
  Logs -> LV.event s e

appEvent :: AppState -> BrickEvent Name AppEvent -> NextState
appEvent s ev = case ev of
    AppEvent (Clock now) -> do
      ppConf <- liftIO $ P.ppConfRelative
      continue $ s & timeL .~ now
                   & buildStateL . BW.ppConfL .~ ppConf
                   & buildsStateL . BSW.ppConfL .~ ppConf
    AppEvent (CIStateUpdate x) -> continue $ s & ciStateL .~ x
                                               & orgsStateL . OSW.listL .~ listReplace (Vector.fromList $ organizations x) (Just 0) (s ^. orgsStateL . OSW.listL)
                                               & buildsStateL . BSW.listL .~ listReplace (Vector.fromList $ lastBuilds x) (Just 0) (s ^. buildsStateL . BSW.listL)
                                               & orgBuildsStateL . OBW.buildsL . BSW.listL .~ listReplace (Vector.fromList $ organizationBuilds x) (Just 0) (s ^. orgBuildsStateL . OBW.buildsL . BSW.listL)
                                               & buildStateL . BW.buildL .~ (build x)
                                               & logsStateL . LW.buildL .~ (build x)
                                               & logsStateL . LW.loglinesL .~ (buildLines x)
    VtyEvent e ->
      case e of
        V.EvKey (V.KChar 'h') [] -> continue $ s & showHelpL %~ not
        V.EvKey V.KEsc [] -> halt s
        V.EvKey (V.KChar 'q') [] -> halt s
        V.EvKey (V.KChar '1') [] -> continue $ s & viewL .~ LastBuilds
        V.EvKey (V.KChar '2') [] -> continue $ s & viewL .~ Orgs

        _ -> do
          case s^.viewL of
            Build ->
              case e of
                V.EvKey V.KDown [] -> vScrollBy logsScroll 1 >> continue s
                V.EvKey V.KUp [] -> vScrollBy logsScroll (-1) >> continue s
                V.EvKey V.KPageDown [] -> vScrollBy logsScroll 10 >> continue s
                V.EvKey V.KPageUp [] -> vScrollBy logsScroll (-10) >> continue s
                _ -> continue s

            LastBuilds ->
              case e of
                V.EvKey V.KEnter [] -> do
                  case listSelectedElement (s^.buildsStateL.BSW.listL) of
                    Nothing -> continue s
                    Just (i, el) -> do
                      liftIO $ atomically $ writeTChan (s^.queryChanL) (A.queryBuild (B.id el))
                      continue $
                        s & viewL .~ Build

                V.EvKey (V.KChar 'r') [V.MCtrl] -> do
                  case listSelectedElement (s^.buildsStateL.BSW.listL) of
                    Nothing -> continue s
                    Just (i, el) -> do
                      liftIO $ atomically $ do
                        writeTChan (s^.queryChanL) (A.queryRestart (B.id el))
                        writeTChan (s^.queryChanL) (A.queryBuild (B.id el))
                        writeTChan (s^.queryChanL) (A.queryWatch (B.id el))
                      continue $
                        s & viewL .~ Logs
                _ -> handleViewEvent s ev

            Orgs ->
              case e of
                V.EvKey V.KEnter [] -> do
                  case listSelectedElement (s^.orgsStateL.OSW.listL) of
                    Nothing -> continue s
                    Just (i, el) -> do
                      liftIO $ atomically $ writeTChan (s^.queryChanL) (A.queryOrganizationBuilds (O.owner el))
                      continue $
                        s & viewL .~ OrgBuilds
                          & orgBuildsStateL . OBW.orgL .~ (Just el)
                _ -> handleViewEvent s ev

            OrgBuilds ->
              case e of
                V.EvKey V.KEnter [] -> do
                  case listSelectedElement (s^.orgBuildsStateL.OBW.buildsL.BSW.listL) of
                    Nothing -> continue s
                    Just (i, el) -> do
                      liftIO $ atomically $ writeTChan (s^.queryChanL) (A.queryBuild (B.id el))
                      continue $
                        s & viewL .~ Build
                _ -> handleViewEvent s ev

            _ -> handleViewEvent s ev
    _ -> continue s

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr $ concat
  [ MV.attrs
  , BW.attrs
  , BSW.attrs ]

theApp :: App AppState AppEvent Name
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const theMap
        }

main :: IO ()
main = do

  b <- newBChan 10 -- bricks BChan which is just hidden TBQueue

  ppConf <- P.ppConfRelative
  now <- getCurrentTime

  (state, updates, queries) <- initScylla
  void . async . forever $ (atomically $ readTChan updates) >>= writeBChan b . CIStateUpdate

  -- clock
  void . async . forever $ getCurrentTime >>= writeBChan b . Clock >> threadDelay 1000000

  void . async $ runScylla state updates queries

  let
      iniState = AppState
        { _buildState = BW.mkState Nothing ppConf
        , _buildsState = BSW.mkState ppConf
        , _orgsState = OSW.mkState
        , _orgBuildsState = OBW.mkState ppConf
        , _logsState = LW.mkState ppConf
        , _ciState = def
        , _queryChan = queries
        , _time = now
        , _showHelp = False
        , _view = LastBuilds
        }

  void $ customMain (V.mkVty V.defaultConfig) (Just b) theApp iniState
