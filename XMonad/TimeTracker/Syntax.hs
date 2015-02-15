
module XMonad.TimeTracker.Syntax where

import Data.List (intercalate)
import qualified Data.Dates as D

import XMonad.TimeTracker.Types

data Expr =
    Lit Value
  | Identifier String
  | List [Expr]
  | StringProperty String
  | Timestamp
  | Duration
  | Func Builtin Expr
  | BinOp BinOp Expr Expr
  | Not Expr
  | Case [(Expr, Expr)] Expr
  deriving (Eq,Show)

data BinOp =
    Equals
  | Match
  | Cut
  | Or
  | And
  | Lt | Gt | Lte | Gte
  deriving (Eq)

instance Show BinOp where
  show Equals = "=="
  show Match = "=~"
  show Cut = "@"
  show Or = "||"
  show And = "&&"
  show Lt = "<"
  show Gt = ">"
  show Lte = "<="
  show Gte = ">="

data Builtin =
    GetWeekDay
  | GetDay
  | GetMonth
  | GetYear
  | GetHour
  | GetMinute
  | GetSecond
  deriving (Eq)

instance Show Builtin where
  show GetWeekDay = "weekday"
  show GetDay = "day"
  show GetMonth = "month"
  show GetYear = "year"
  show GetHour = "hour"
  show GetMinute = "minute"
  show GetSecond = "second"

pPrint :: Expr -> String
pPrint (Lit value) = toString value
pPrint (Identifier id) = id
pPrint (List es) = "[" ++ intercalate ", " (map pPrint es) ++ "]"
pPrint (StringProperty p) = "$" ++ p
pPrint Timestamp = "$timestamp"
pPrint Duration = "$duration"
pPrint (BinOp op e1 e2) = pPrint e1 ++ show op ++ pPrint e2
pPrint (Func p e) = show p ++ " " ++ pPrint e
pPrint (Not e) = "!" ++ pPrint e
pPrint (Case pairs def) = "case " ++ unlines (map go pairs) ++ " else " ++ pPrint def
  where
    go (cond, val) = " when " ++ pPrint cond ++ " then " ++ pPrint val

duration2int :: D.Time -> Int
duration2int (D.Time h m s) = 3600*h + 60*m + s

data Value =
    String String
  | Int Int
  | Bool Bool
  | DateTime D.DateTime
  | Time D.Time
  | WeekDay D.WeekDay
  | LitList [Value]
  deriving (Eq, Ord, Show)

matchAny :: Expr -> [String] -> Expr
matchAny expr regexs = BinOp Match expr $ List $ map (Lit . String) regexs

isElem :: Expr -> [String] -> Expr
isElem expr regexs = BinOp Equals expr $ List $ map (Lit . String) regexs

toBool :: Value -> Bool
toBool (Bool b) = b
toBool (Int n) = n /= 0
toBool (String s) = s /= "" 
toBool (LitList lst) = not (null lst)
toBool (DateTime _) = True
toBool (Time t) = (D.tHour t /= 0) || (D.tMinute t /= 0) || (D.tSecond t /= 0)

toString :: Value -> String
toString (String s) = s
toString (Int n) = show n
toString (Bool b) = show b
toString (LitList [x]) = toString x
toString (LitList xs) = error $ "Unsupported nested list: " ++ show xs
toString (DateTime t) = show t
toString (Time t) = show t
toString (WeekDay w) = show w

toStrings :: Value -> [String]
toStrings (String s) = [s]
toStrings (Int n) = [show n]
toStrings (Bool b) = [show b]
toStrings (LitList xs) = concatMap toStrings xs
toStrings (DateTime t) = [show t]
toStrings (Time t) = [show t]
toStrings (WeekDay w) = [show w]

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

