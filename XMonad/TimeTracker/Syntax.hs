
module XMonad.TimeTracker.Syntax where

import XMonad.TimeTracker.Types

data Expr =
    Lit Value
  | Identifier String
  | List [Expr]
  | StringProperty String
  | Equals Expr Expr
  | Match Expr Expr
  | Or Expr Expr
  | And Expr Expr
  deriving (Eq,Show)

data Value =
    String String
  | Bool Bool
  deriving (Eq, Ord, Show)

matchAny :: Expr -> [String] -> Expr
matchAny expr regexs = Match expr $ List $ map (Lit . String) regexs

isElem :: Expr -> [String] -> Expr
isElem expr regexs = Equals expr $ List $ map (Lit . String) regexs

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

data VarDefinition = VarDefinition String Expr
  deriving (Eq, Show)

data QueryDefinition = QueryDefinition String Query
  deriving (Eq, Show)

data Definitions =
    Definitions {
      dVariables :: [(String, Expr)],
      dQueries :: [(String, Query)]
    }
  deriving (Eq, Show)

getStringProperty :: String -> TEvent -> String
getStringProperty "task" e = eTask e
getStringProperty "title" e = eWindowTitle e
getStringProperty "class" e = eWindowClass e
getStringProperty "workspace" e = eWorkspace e
getStringProperty p _ = error $ "Unknown property: " ++ p

