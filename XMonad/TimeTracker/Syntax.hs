
module XMonad.TimeTracker.Syntax where

import Data.List (intercalate)

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

pPrint :: Expr -> String
pPrint (Lit value) = toString value
pPrint (Identifier id) = id
pPrint (List es) = "[" ++ intercalate ", " (map pPrint es) ++ "]"
pPrint (StringProperty p) = "$" ++ p
pPrint (Equals e1 e2) = pPrint e1 ++ "==" ++ pPrint e2
pPrint (Match e1 e2) = pPrint e1 ++ "=~" ++ pPrint e2
pPrint (Or e1 e2) = pPrint e1 ++ "||" ++ pPrint e2
pPrint (And e1 e2) = pPrint e1 ++ "&&" ++ pPrint e2

data Value =
    String String
  | Bool Bool
  | LitList [Value]
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
toString (LitList xs) = error $ "Unsupported nested list: " ++ show xs

toStrings :: Value -> [String]
toStrings (String s) = [s]
toStrings (Bool b) = [show b]
toStrings (LitList xs) = concatMap toStrings xs

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
      dVariables :: [VarDefinition],
      dQueries :: [QueryDefinition]
    }
  deriving (Eq, Show)

getQuery :: String -> Definitions -> Maybe Query
getQuery name ds =
  case [q | QueryDefinition n q <- dQueries ds, n == name] of
    [] -> Nothing
    qs -> Just $ last qs

getStringProperty :: String -> TEvent -> String
getStringProperty "task" e = eTask e
getStringProperty "title" e = eWindowTitle e
getStringProperty "class" e = eWindowClass e
getStringProperty "workspace" e = eWorkspace e
getStringProperty p _ = error $ "Unknown property: " ++ p

