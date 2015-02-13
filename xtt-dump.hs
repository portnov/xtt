
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get
import System.Environment (getArgs)
import System.IO

import XMonad.TimeTracker

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of  
                [] -> return "tracker.dat"
                [name] -> return name
                _ -> fail $ "Synopsis: xtt-dump [filename.dat]"
  dat <- BL.readFile filename
  let events = runGet readEvents dat
  forM_ events $ \ev -> print ev
    
