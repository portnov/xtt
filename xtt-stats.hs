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
import Options.Applicative
import qualified Text.Parsec as Parsec
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

data Options =
  Options {
    oLogFilename :: FilePath,
    oDefsFilename :: FilePath,
    oQuery :: Maybe String,
    oQueryName :: String
  }
  deriving (Eq, Show)

cmdline :: FilePath -> FilePath -> Parser Options
cmdline defLog defDefs =
  Options
    <$> strOption
        (long "data" <> metavar "TRACKER.DAT"
         <> short 'd'
         <> value defLog
         <> help "Tracker data log file")
    <*> strOption
        (long "source" <> metavar "SOURCE.XTT"
        <> short 's'
        <> value defDefs
        <> help "Query definitions source file")
    <*> optional (strOption
          (long "query" <> metavar "QUERY"
           <> short 'q'
           <> help "Execute specified query instead of default"))
    <*> argument str (metavar "QUERY" <> value "default")

main :: IO ()
main = do
    defLog <- defaultTrackerLog
    defDefs <- defaultDefSource
    let opts = info (helper <*> cmdline defLog defDefs)
                    (fullDesc
                     <> progDesc "Output results of QUERY defined in SOURCE.XTT by tracker data log TRACKER.DAT"
                     <> header "xtt-stats - statistics tool of XMonad Time Tracker")
    execParser opts >>= realMain

realMain :: Options -> IO ()
realMain opts = do
  dat <- BL.readFile (oLogFilename opts)
  let events = runGet readEvents dat
  defs <- parseFile (oDefsFilename opts)

  qry <- case oQuery opts of
           Just qryText -> case Parsec.parse pQuery "<command line>" qryText of
                             Left err -> fail $ show err
                             Right q -> return q
           Nothing -> case getQuery (oQueryName opts) defs of
                        Nothing -> fail "No specified query defined"
                        Just q -> return q

  let (tasks, total) = runProcess (dVariables defs) qry events
  forM_ (M.assocs tasks) $ \(key, ti) -> do
           putStrLn $ toString key ++ "\t" ++ formatTaskInfo ti
  putStrLn $ "Total: " ++ formatDt total

