module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

data QType = TQubit | TBit | TFunction [QType] QType
  deriving (Show, Eq)

data G = H | X | Y | Z | T | S | CX | Measure
  deriving (Show, Eq)

data N
  | Var String
  | Apply N N
  | Lambda String QType N
  | Let String N N
  | Sequence [N]
  | Gate G
  deriving (Eq, Show)

{-

apply_expr come fallback generico, da gestire ambiguita

expr        ::= lambda

              | let_expr

              | sequence

              | apply_expr


lambda      ::= "lambda" identifier ":" qtype "->" expr

let_expr    ::= "let" identifier "=" expr "in" expr

sequence    ::= "{" expr (";" expr)* "}" --{H q_1;CX q_1 q_2}


Associativita a sinistra, foldl potrebbe essere utile, H q_1, H bell q_2

apply_expr  ::= atom (atom)*


Parsing atomo 

atom        ::= identifier 

              | gate      

              | "(" expr ")"


gate        ::= "H" | "X" | "Y" | "Z" | "T" | "S" | "CX" | "Measure"


Tipi parametrizzati per le lambda

qtype       ::= "Qubit" 

              | "Bit" 

              | "(" qtype "->" qtype ")"
 -}


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



identifierParser :: Parser N
identifierParser = do
    spaces
    v <- many1 letter
    spaces
    --notFollowedBy alphaNum
    return (Var v)


-- Per i Gate e i tipi non penso serva un parser intero
atomParser :: Parser N
atomParser = try identifierParser <|> try gateParser <|> try parensParser

exprParser :: Parser N
exprParser = try letParser <|> try lambdaParser <|> try sequenceParser <|> applyParser

-----


main :: IO ()
main = do
    putStrLn "Inserisci l'espressione:let bell = lambda q_1.let q_2 = H q_1 in Measure q_2 in bell q"
    input <- getLine
    case parse exprParser "" input of
        Left err  -> putStrLn ("Errore di parsing:\n" ++ show err)
        Right res -> putStrLn ("Input parsato:\n" ++ show res)

