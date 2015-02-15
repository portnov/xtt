
module XMonad.TimeTracker.Parser where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Reader
import Data.Either
import qualified Data.Dates as D
import Text.Parsec hiding (runParser)
-- import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
-- import Text.Parsec.Language (haskellDef)

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Syntax

xttDef :: P.GenLanguageDef String () (Reader D.DateTime)
xttDef = P.LanguageDef {
           P.commentStart = "{-",
           P.commentEnd = "-}",
           P.commentLine = "--",
           P.nestedComments = True,
           P.identStart = letter,
           P.identLetter = alphaNum <|> oneOf "_'",
           P.opStart = P.opLetter xttDef,
           P.opLetter = oneOf ":!#$%*+./<=>?@\\^|-~",
           P.reservedOpNames = ["==", "=~", "&&", "||", "!", "!=", "@"],
           P.reservedNames = ["let", "query", "select", "where", "group", "by", "as", "case", "when", "then",
                              "weekday", "day", "month", "year", "hour", "minute", "second"],
           P.caseSensitive = True
        }

xttTokenParser :: P.GenTokenParser String () (Reader D.DateTime)
xttTokenParser = P.makeTokenParser xttDef

parens = P.parens xttTokenParser
brackets = P.brackets xttTokenParser
identifier = P.identifier xttTokenParser
stringLiteral = P.stringLiteral xttTokenParser
whitespace = P.whiteSpace xttTokenParser
symbol = P.symbol xttTokenParser
comma = P.comma xttTokenParser
semicolon = P.semi xttTokenParser
reservedOp = P.reservedOp xttTokenParser
reserved = P.reserved xttTokenParser
number = P.decimal xttTokenParser
lexeme = P.lexeme xttTokenParser

type Parser a = ParsecT String () (Reader D.DateTime) a

pValue :: Parser Value
pValue =
  try (String <$> regex1Literal <?> "regex literal 1") <|>
  try (String <$> regex2Literal <?> "regex literal 2") <|>
  try (DateTime <$> dateTimeLiteral <?> "date/time literal") <|>
  try (Time <$> timeLiteral <?> "time literal") <|>
  try (Time <$> durationLiteral <?> "duration literal") <|>
  try (Int <$> integerLiteral <?> "integer literal") <|>
  try (WeekDay <$> weekdayLiteral <?> "weekday literal") <|>
  try (String <$> stringLiteral) <|>
  (Bool <$> (boolLiteral <?> "boolean literal"))

integerLiteral :: Parser Int
integerLiteral = fromIntegral <$> lexeme number

weekdayLiteral :: Parser D.WeekDay
weekdayLiteral =
        use D.Monday <|> use D.Tuesday <|> use D.Wednesday
    <|> use D.Thursday <|> use D.Friday <|> use D.Saturday <|> use D.Sunday
  where
    use wd = try (symbol (show wd) >> return wd)

dateTimeLiteral :: Parser D.DateTime
dateTimeLiteral = lexeme (ask >>= D.pDateTime)

durationLiteral :: Parser D.Time
durationLiteral = do
    ds <- many1 step
    whitespace
    return $ foldr plus (D.Time 0 0 0) ds
  where
    plus (D.Time h1 m1 s1) (D.Time h2 m2 s2) = {- normalizeTime $ -} D.Time (h1+h2) (m1+m2) (s1+s2)

    step = do
      n <- fromIntegral <$> number
      c <- oneOf "hms"
      case c of
        'h' -> return $ D.Time n 0 0
        'm' -> return $ D.Time 0 n 0
        's' -> return $ D.Time 0 0 n
        _ -> fail "Impossible"

timeLiteral :: Parser D.Time
timeLiteral = lexeme D.pTime

regex1Literal :: Parser String
regex1Literal = do
  char '/'
  res <- manyTill anyChar (try $ char '/')
  whitespace
  return res

regex2Literal :: Parser String
regex2Literal = do
  char 'm'
  sep <- anyChar
  res <- manyTill anyChar (char sep)
  whitespace
  return res

boolLiteral :: Parser Bool
boolLiteral =
    try (trueStrings >> return True) <|>
    (falseStrings >> return False)
  where
    trueStrings = choice $ map try $ map symbol ["True", "true", "yes", "on", "1"]
    falseStrings = choice $ map try $ map symbol ["False", "false", "no", "off", "0"]

pStringProperty :: Parser Expr
pStringProperty = do
  char '$'
  name <- choice $ map try $ map symbol ["task", "title", "class", "workspace"]
  return $ StringProperty name

pLiteral :: Parser Expr
pLiteral = Lit <$> pValue

pList :: Parser Expr
pList = List <$> (brackets $ pExpr `sepBy` comma)

pIdentifier :: Parser Expr
pIdentifier = Identifier <$> identifier

pExpr :: Parser Expr
pExpr =
  try pCase <|>
  buildExpressionParser table term <?> "expression"

pFunction :: Parser Expr
pFunction = do
        use "weekday" GetWeekDay
    <|> use "day" GetDay
    <|> use "month" GetMonth
    <|> use "year" GetYear
    <|> use "hour" GetHour
    <|> use "minute" GetMinute
    <|> use "second" GetSecond
  where
    use str func = try $ do
      reserved str
      expr <- term
      return $ Func func expr

pCase :: Parser Expr
pCase = do
    reserved "case"
    pairs <- many1 pWhen
    reserved "else"
    def <- pExpr
    return $ Case pairs def
  where
    pWhen = do
      reserved "when"
      cond <- pExpr
      reserved "then"
      expr <- pExpr
      return (cond, expr)

pTimestamp = do
  char '$'
  symbol "timestamp"
  return Timestamp

pDuration = do
  char '$'
  symbol "duration"
  return Duration

term = parens pExpr
   <|> pList
   <|> pLiteral
   <|> pFunction
   <|> pIdentifier
   <|> (try pTimestamp <?> "$timestamp")
   <|> (try pDuration <?> "$duration")
   <|> pStringProperty
   <?> "simple expression"

table = [ [prefix "!" Not],
          [binary "@" Cut AssocNone],
          [binary "=~" Match AssocNone, binary "==" Equals AssocNone,
           binary "<=" Lte AssocNone, binary "<" Lt AssocNone,
           binary ">=" Gte AssocNone, binary ">" Gt AssocNone,
           binaryNot "!=" Equals AssocNone, binaryNot "/=" Equals AssocNone],
          [binary "||" Or AssocLeft, binary "&&" And AssocLeft] ]

binary name fun assoc = Infix (try (reservedOp name) >> return (\e1 e2 -> BinOp fun e1 e2)) assoc
binaryNot name fun assoc = Infix (try (reservedOp name) >> return (\e1 e2 -> Not (BinOp fun e1 e2))) assoc
prefix name fun = Prefix (try (reservedOp name) >> return fun)

pVarDefinition :: Parser VarDefinition
pVarDefinition = do
  reserved "let"
  name <- identifier
  reservedOp "="
  expr <- pExpr
  return $ VarDefinition name expr

pQuery :: Parser Query
pQuery = do
  where_ <- option (Lit $ Bool True) $ do
                reserved "where"
                pExpr
  reserved "group"
  reserved "by"
  grouping <- pGrouping `sepBy` comma
  when (null grouping) $
      fail "At least one group by parameter must be specified"
  let ((_,top):other) = grouping
  return $ Query other where_ top

pGrouping :: Parser (String, Expr)
pGrouping = do
  expr <- pExpr
  name <- option (pPrint expr) $ do
             reserved "as"
             identifier
  return (name, expr)

pQueryDefinition :: Parser QueryDefinition
pQueryDefinition = do
  reserved "query"
  name <- identifier
  reservedOp "="
  qry <- pQuery
  return $ QueryDefinition name qry

pDefinitions :: Parser Definitions
pDefinitions = do
  whitespace
  lst <- ((Left <$> try pVarDefinition) <|>
          (Right <$> try pQueryDefinition))
         `sepEndBy` semicolon
  eof
  return $ Definitions {
             dVariables = lefts lst,
             dQueries   = rights lst
           }

runTest :: Show a => Parser a -> String -> IO ()
runTest p str = do
  t <- D.getCurrentDateTime
  case runReader (runParserT p () "<input>" str) t of
    Right r -> print r
    Left err -> fail $ show err

testExpr :: String -> IO ()
testExpr = runTest (do {e <- pExpr ; eof ; return e})

testQry :: String -> IO ()
testQry = runTest (do {e <- pQueryDefinition ; eof ; return e})

runParser :: Parser a -> FilePath -> String -> IO a
runParser p path str = do
  t <- D.getCurrentDateTime
  case runReader (runParserT p () path str) t of
    Left err -> fail $ show err
    Right ds -> return ds

parseFile :: FilePath -> IO Definitions
parseFile path = do
  input <- readFile path
  runParser pDefinitions path input

