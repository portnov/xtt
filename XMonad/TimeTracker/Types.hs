{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module XMonad.TimeTracker.Types where

import Data.Typeable
import Data.Time
import Data.Binary as Binary
import GHC.Generics (Generic)

data TEvent = TEvent {
       eTimestamp :: UTCTime
     , eTask :: String
     , eWindowTitle :: String
     , eWindowClass :: String
     , eWorkspace :: String
     }
   | Quit
   deriving (Show, Generic, Typeable)

instance Eq TEvent where
  e1 == e2 =
    (eTask e1 == eTask e2) &&
    (eWindowTitle e1 == eWindowTitle e2) &&
    (eWindowClass e1 == eWindowClass e2) &&
    (eWorkspace e1 == eWorkspace e2)

instance Binary UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        Binary.put d
        Binary.put (toRational t)
 get = do
        d <- Binary.get
        t <- Binary.get
        return $ UTCTime (ModifiedJulianDay d) ({-# SCC diffTimeFromRational #-} fromRational t)

instance Binary TEvent

