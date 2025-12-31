module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

data N
  = LitInt Int
  | Var String
  | Add N N
  | Mul N N
  | Let String N N
  deriving (Eq, Show)

type Prog = [N]

---Parsing Atomi
intParser :: Parser N
intParser = do
    spaces
    ds <- (many1 digit)
    spaces
    --notFollowedBy alphaNum 
    spaces
    return (LitInt (read ds))

varParser :: Parser N
varParser = do
    spaces
    v <- many1 letter
    spaces
    --notFollowedBy alphaNum
    return (Var v)

atomParser :: Parser N
atomParser = try intParser <|> varParser

-----


parensParser :: Parser N
parensParser = do
  spaces
  char '('
  spaces
  e1 <- exprParser
  spaces
  op <- (char '+' >> return Add) <|> (char '*' >> return Mul)
  spaces
  e2 <- exprParser
  spaces
  char ')'
  spaces
  return (op e1 e2)

letParser :: Parser N
letParser = do
    spaces
    char '('
    spaces
    string "let"
    spaces
    v <- many1 letter
    spaces
    char '='
    spaces
    e1 <- exprParser 
    spaces
    string "in"
    spaces
    e2 <- exprParser
    spaces
    char ')'
    spaces
    return(Let v e1 e2)


exprParser :: Parser N
exprParser = try letParser <|> try parensParser <|> atomParser

main :: IO ()
main = do
    putStrLn "Inserisci l'espressione try :((l*4) + (let x = 3 in (x\t+2)))"
    input <- getLine
    case parse exprParser "" input of
        Left err  -> putStrLn ("Errore di parsing:\n" ++ show err)
        Right res -> putStrLn ("Input parsato:\n" ++ show res)

