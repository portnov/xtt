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
import XMonad.TimeTracker.Parser
import XMonad.TimeTracker

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
  defs <- parseFile "sample.xtt"
  case getQuery "qry2" defs of
    Nothing -> putStrLn "No default query"
    Just qry -> do
          let tasks = runProcess (dVariables defs) qry events
          forM_ (M.assocs tasks) $ \(key, ti) -> do
              putStrLn $ toString key ++ "\t" ++ formatTaskInfo ti

