
module XMonad.TimeTracker.Parser where

import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Syntax

xttDef = haskellDef {
           P.reservedOpNames = ["==", "=~", "&&", "||"],
           P.reservedNames = ["let", "query", "select", "where", "group", "by"]
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
reservedOp = P.reservedOp xttTokenParser

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

testExpr :: String -> IO ()
testExpr = parseTest (do {e <- pExpr ; eof ; return e})

