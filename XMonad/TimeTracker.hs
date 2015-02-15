{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module XMonad.TimeTracker where

import Control.Monad
import qualified Control.Exception as E
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Int
import Data.Typeable
import Data.Time
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO
import Data.Binary as Binary
import Data.Binary.Get (isEmpty)
import GHC.Generics (Generic)
import Text.Regex.Posix
import Graphics.X11.XScreenSaver

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Actions.ShowText

import XMonad.TimeTracker.Types

data Tracker = Tracker {
      trackerChan :: TChan TEvent,
      trackerTask :: String
    }
    | NoTracker
    deriving (Typeable)

instance ExtensionClass Tracker where
  initialValue = NoTracker

defaultFile :: FilePath -> IO FilePath
defaultFile name = do
  home <- getEnv "HOME"
  return $ home </> ".xmonad" </> name

defaultTrackerLog :: IO FilePath
defaultTrackerLog = defaultFile "tracker.dat"

defaultDefSource :: IO FilePath
defaultDefSource = defaultFile "query.xtt"

trackerInit :: FilePath -> X ()
trackerInit path = do
  chan <- io $ atomically $ newTChan
  file <- io $ openFile path AppendMode
  io $ forkIO $ writer chan file
--   withDisplay $ \dpy ->
--       io $ forkIO $ idleTracker dpy chan
  let tracker = Tracker chan "Startup"
  XS.put tracker

idleTracker :: Display -> TChan TEvent -> IO ()
idleTracker dpy chan = go
  where
    go = do
      idle <- getXIdleTime dpy
      when (idle >= 1000) $ do
          currentZone <- getCurrentTimeZone
          time <- getCurrentTime
          atomically $ writeTChan chan $ IdleEvent (ZonedUTC time currentZone) idle
      threadDelay (60 * 1000 * 1000)
      go

writer :: TChan TEvent -> Handle -> IO ()
writer chan file = go Nothing
  where
    go lastEv = do
      ev <- atomically $ readTChan chan
      case ev of
        Quit -> hClose file
        _ -> do
             when (Just ev /= lastEv) $ do
               BL.hPut file $ encode ev
               hFlush file
             go (Just ev)

trackerHook :: X ()
trackerHook = do
  currentZone <- io getCurrentTimeZone
  tracker <- XS.get
  let chan = trackerChan tracker
  idle <- withDisplay $ \d -> io (getXIdleTime d)
  withWindowSet $ \ss -> do
    whenJust (W.peek ss) $ \window -> do
      time <- io $ getCurrentTime
      cls <- runQuery className window
      winTitle <- runQuery title window
      let event = BaseEvent {
                    eTimestamp = ZonedUTC time currentZone,
                    eWindowTitle = winTitle,
                    eWindowClass = cls,
                    eAtoms = [],
                    eIdleTime = idle,
                    eWorkspace = W.currentTag ss }
      io $ atomically $ writeTChan chan event

trackerSetTask :: String -> X ()
trackerSetTask task = do
  tracker <- XS.get
  let chan = trackerChan tracker
  let event = SetMeta "task" task
  io $ atomically $ writeTChan chan event

promptTrackerTask :: XPConfig -> X ()
promptTrackerTask xpc = do
  x <- inputPrompt xpc "Task"
  whenJust x $ \task -> do
    trackerSetTask task

readEvList :: Binary a => Binary.Get [a]
readEvList = do
  empty <- isEmpty
  if empty
    then return []
    else do
         ev <- Binary.get
         rest <- readEvList
         return (ev : rest)

matchOne :: String -> String -> Maybe String
matchOne title regex =
  let (before, r, after, groups) = title =~ regex :: (String, String, String, [String])
  in  if null groups
        then Nothing
        else Just (head groups)

matchMultiple :: String -> [String] -> Maybe String
matchMultiple title regexs = msum [title `matchOne` regex | regex <- regexs]

grabWindowTitle :: [String] -> X ()
grabWindowTitle regexs = do
  withWindowSet $ \ss -> do
    whenJust (W.peek ss) $ \window -> do
    winTitle <- runQuery title window
    whenJust (matchMultiple winTitle regexs) $ \task -> do
        trackerSetTask task
        flashText (def :: ShowTextConfig) 3 task

