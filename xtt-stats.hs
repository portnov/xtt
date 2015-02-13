module Main where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import Data.Time
import Data.Binary
import Data.Binary.Get
import System.Environment (getArgs)
import System.IO
import Text.Regex.Posix

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Eval
import XMonad.TimeTracker.Syntax
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


formatDt :: NominalDiffTime -> String
formatDt dt = 
    let ns = go (floor dt) [60, 60, 24]
    in  (unwords $ reverse $ zip0 ns "smh") -- ++ " (" ++ show dt ++ ")"
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

qry :: Query
qry = Query {
  qSelect = [],
  qWhere = Or (StringProperty "title" `MatchAny` workTitles) $
           Or (StringProperty "workspace" `MatchAny` workWorkspaces) $
           StringProperty "class" `In` map (Lit . String) workClasses,
  qGroupBy = StringProperty "task"
}

qry2 :: Query
qry2 = qry {qSelect = [("TITLE", StringProperty "title"), ("CLASS", StringProperty "class")]}

qry3 :: Query
qry3 = Query {
  qSelect = [("TITLE", StringProperty "title")],
  qWhere = Lit (Bool True),
  qGroupBy = StringProperty "workspace"
}

formatTaskInfo :: TaskInfo -> String
formatTaskInfo ti =
    formatDt (tiDuration ti) ++ "\n" ++
    unlines (concatMap showValues $ M.assocs $ tiFields ti)
  where
    showValues (key, vmap) =
        ["\t" ++ key ++ ":\t" ++ toString value ++ "\t" ++ formatDt dt  | (value, dt) <- M.assocs vmap]

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of  
                [] -> defaultTrackerLog
                [name] -> return name
                _ -> fail $ "Synopsis: xtt-stats [filename.dat]"
  dat <- BL.readFile filename
  let events = runGet readEvents dat
  let tasks = runProcess qry2 events
  forM_ (M.assocs tasks) $ \(key, ti) -> do
      putStrLn $ toString key ++ "\t" ++ formatTaskInfo ti

