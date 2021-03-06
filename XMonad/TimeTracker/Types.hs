{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving #-}

module XMonad.TimeTracker.Types where

import Control.Applicative
import Data.Typeable
import Data.Time
import Data.Binary as Binary
import GHC.Generics (Generic)

deriving instance Generic TimeZone

instance Binary TimeZone

data ZonedUTC =
    ZonedUTC {
      getUTC :: UTCTime,
      getTimeZone :: TimeZone
    }
  deriving (Eq, Show, Generic)

data TEvent =
     BaseEvent {
       eTimestamp :: ZonedUTC
     , eWindowTitle :: String
     , eWindowClass :: String
     , eWorkspace :: String
     , eAtoms :: [(String, String)]
     , eIdleTime :: Int
     }
   | SetMeta String String
   | Quit
   | IdleEvent {
       eTimestamp :: ZonedUTC
     , eIdleTime :: Int
     }
   deriving (Show, Generic, Typeable)

instance Eq TEvent where
  SetMeta k1 v1 == SetMeta k2 v2 = k1 == k2 && v1 == v2
  Quit == Quit = True
  e1@(BaseEvent {}) == e2@(BaseEvent {}) =
    (eWindowTitle e1 == eWindowTitle e2) &&
    (eWindowClass e1 == eWindowClass e2) &&
    (eWorkspace e1 == eWorkspace e2) &&
    (eAtoms e1 == eAtoms e2) &&
    (eIdleTime e1 == eIdleTime e2)
  (IdleEvent _ it1) == (IdleEvent _ it2) = it1 == it2 
  e1 == e2 = False

instance Binary Day where
  put (ModifiedJulianDay d) = Binary.put d
  get = ModifiedJulianDay <$> get

instance Binary UTCTime where
 put (UTCTime d t) = do
        Binary.put d
        Binary.put (toRational t)
 get = do
        d <- Binary.get
        t <- Binary.get
        return $ UTCTime d ({-# SCC diffTimeFromRational #-} fromRational t)

instance Binary ZonedUTC

instance Binary TEvent



