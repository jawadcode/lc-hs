{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Expr
      ( EVar,
        EIntLit,
        EStrLit,
        EFun,
        ELetIn,
        ENeg,
        EAdd,
        ESub,
        EMul,
        EDiv
      ),
    exprParser,
  )
where

import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    Parsec,
    between,
    choice,
    many,
    manyTill,
    (<?>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as Lex

data Expr
  = EIntLit Int
  | EVar String
  | EStrLit String
  | EFun (NonEmpty String) Expr
  | EApp Expr Expr
  | ELetIn (NonEmpty (String, Expr)) Expr
  | ENeg Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EPow Expr Expr
  deriving (Show)

type Parser = Parsec Void Text

exprParser :: Parser Expr
exprParser = expr <* eof

expr :: Parser Expr
expr = choice [makeExprParser term opTable, lett, fun]

lett :: Parser Expr
lett =
  liftA2
    ELetIn
    (keyword "let" *> bindings)
    (keyword "in" *> expr)
  where
    bindings =
      NE.sepBy1
        (liftA2 (,) ident $ symbol "=" *> expr)
        (symbol ",")

fun :: Parser Expr
fun = keyword "fun" *> liftA2 EFun (NE.some ident) (symbol "=>" *> expr)

term :: Parser Expr
term = choice [parens expr, str, var, int]
  where
    parens = between (symbol "(") (symbol ")")

opTable :: [[Operator Parser Expr]]
opTable =
  [ [infixLeft "" EApp],
    [infixRight "^" EPow],
    [ prefix "-" ENeg
    ],
    [ infixLeft "*" EMul,
      infixLeft "/" EDiv
    ],
    [ infixLeft "+" EAdd,
      infixLeft "-" ESub
    ]
  ]

infixLeft :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
infixLeft name f = InfixL (f <$ symbol name)

infixRight :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
infixRight name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy alphaNumChar)

var :: Parser Expr
var = EVar <$> ident

kws :: [String]
kws = ["let", "in", "fun"]

ident :: Parser String
ident = (lexeme . try) (parser >>= check) <?> "identifier"
  where
    parser = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` kws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

int :: Parser Expr
int = EIntLit <$> lexeme Lex.decimal

str :: Parser Expr
str = char '"' *> (EStrLit <$> manyTill Lex.charLiteral (char '"'))

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme ws

symbol :: Text -> Parser Text
symbol = Lex.symbol ws

ws :: Parser ()
ws =
  Lex.space
    space1
    (Lex.skipLineComment "#")
    (Lex.skipBlockComment "(*" "*)")
