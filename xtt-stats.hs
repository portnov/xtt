module Main where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Data.Time
import Data.Binary
import Data.Binary.Get
import System.Environment (getArgs)
import System.IO
import Text.Regex.Posix

import XMonad.TimeTracker

workTitles =
  ["Remmina", "RadixWare Designer",
   "Oracle SQL Developer",
   "RadixWare Manager", "NetBeans", "Compass Plus Ltd"]

workWorkspaces =
  ["work", "RX Explorer"]

workClasses =
  ["Qt Jambi application", "Remmina"]

isWork :: TEvent -> Bool
isWork ev =
  or [eWindowTitle ev =~ regex | regex <- workTitles] ||
  or [eWorkspace ev =~ regex | regex <- workWorkspaces] ||
  or [eWindowClass ev == regex | regex <- workClasses]

data ParserState = PS {
      psCurrentTask :: String,
      psStartTime :: Maybe UTCTime,
      psPrevEvent :: Maybe TEvent,
      psTasks :: M.Map String NominalDiffTime
    } deriving (Eq, Show)

emptyPS :: ParserState
emptyPS = PS {
  psCurrentTask = "Startup",
  psStartTime = Nothing,
  psPrevEvent = Nothing,
  psTasks = M.empty }

putTime :: String -> NominalDiffTime -> St.State ParserState ()
putTime task dt = do
  St.modify $ \st ->
    case M.lookup task (psTasks st) of
      Nothing -> st {psTasks = M.insert task dt (psTasks st)}
      Just odt -> st {psTasks = M.insert task (dt+odt) (psTasks st)}

process :: TEvent -> St.State ParserState ()
process ev = do
  st <- St.get
  case psPrevEvent st of
    Nothing -> return ()
    Just prevEvent -> do
      let dt = diffUTCTime (eTimestamp ev) (eTimestamp prevEvent)
      when (isWork prevEvent) $
          putTime (psCurrentTask st) dt
  when (eTask ev /= psCurrentTask st) $ do
      St.put $ st {psCurrentTask = eTask ev, psStartTime = Just (eTimestamp ev)}
  St.modify $ \st -> st {psPrevEvent = Just ev}

processAll :: [TEvent] -> St.State ParserState ()
processAll events = mapM_ process events

runProcess :: [TEvent] -> M.Map String NominalDiffTime
runProcess events = 
  let st = St.execState (processAll events) emptyPS
  in  psTasks st

formatDt :: NominalDiffTime -> String
formatDt dt = 
    let ns = go (floor dt) [60, 60, 24]
    in  (unwords $ reverse $ zip0 ns "smh") ++ " (" ++ show dt ++ ")"
    -- in  show ns 
  where
    go n [] = [n]
    go n (p:ps) =
        let (d,m) = divMod n p
        in  m : go d ps

    zip0 [] _ = []
    zip0 _ [] = []
    zip0 (0:ns) (_:cs) = zip0 ns cs
    zip0 (n:ns) (c:cs) = (show n ++ [c]) : zip0 ns cs

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of  
                [] -> return "tracker.dat"
                [name] -> return name
                _ -> fail $ "Synopsis: xtt-stats [filename.dat]"
  dat <- BL.readFile filename
  let events = runGet readEvents dat
  let tasks = runProcess events
  forM_ (M.assocs tasks) $ \(task, dt) -> do
      putStrLn $ task ++ "\t" ++ formatDt dt

