
module XMonad.TimeTracker.Parser where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Either
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Syntax

xttDef = haskellDef {
           P.reservedOpNames = ["==", "=~", "&&", "||"],
           P.reservedNames = ["let", "query", "select", "where", "group", "by", "as"]
        }

xttTokenParser :: P.TokenParser ()
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

pValue :: Parser Value
pValue =
  try (String <$> stringLiteral) <|>
    (Bool <$> (boolLiteral <?> "boolean literal"))

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
pExpr = buildExpressionParser table term <?> "expression"

term = parens pExpr
   <|> pList
   <|> pLiteral
   <|> pIdentifier
   <|> pStringProperty
   <?> "simple expression"

table = [ [binary "=~" Match AssocNone, binary "==" Equals AssocNone],
          [binary "||" Or AssocLeft, binary "&&" And AssocLeft] ]

binary name fun assoc = Infix (try (reservedOp name) >> return fun) assoc

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

testExpr :: String -> IO ()
testExpr = parseTest (do {e <- pExpr ; eof ; return e})

testQry :: String -> IO ()
testQry = parseTest (do {e <- pQueryDefinition ; eof ; return e})

parseFile :: FilePath -> IO Definitions
parseFile path = do
  input <- readFile path
  case parse pDefinitions path input of
    Left err -> fail $ show err
    Right ds -> return ds

