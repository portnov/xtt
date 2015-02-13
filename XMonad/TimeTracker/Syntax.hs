
module XMonad.TimeTracker.Syntax where

import Text.Parsec

import XMonad.TimeTracker.Types

data Expr =
    Lit Value
  | StringProperty String
  | Equals Expr Expr
  | Match Expr String
  | MatchAny Expr [String]
  | In Expr [Expr]
  | Or Expr Expr
  | And Expr Expr
  deriving (Eq,Show)

data Value =
    String String
  | Bool Bool
  deriving (Eq, Ord, Show)

toBool :: Value -> Bool
toBool (Bool b) = b
toBool (String s) = s /= "" 

toString :: Value -> String
toString (String s) = s
toString (Bool b) = show b

data Query =
  Query {
      qSelect :: [(String, Expr)] -- ^ (Title, expression)
    , qWhere :: Expr
    , qGroupBy :: Expr
  }
  deriving (Eq, Show)

getStringProperty :: String -> TEvent -> String
getStringProperty "task" e = eTask e
getStringProperty "title" e = eWindowTitle e
getStringProperty "class" e = eWindowClass e
getStringProperty "workspace" e = eWorkspace e
getStringProperty p _ = error $ "Unknown property: " ++ p

