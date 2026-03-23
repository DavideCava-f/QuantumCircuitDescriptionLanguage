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

              | "(" qtype "to" qtype ")"

Quanto sono importanti i tipi, quali devo mettere?
Lo spazio di nomi come lo devo gestire?
Perche usare coppie di \otimes al posto di sequenze, \otimes non si usa per sistemi indipendenti?

T:: bit qbit T to T T tensor t

termini = applicazioni di temini(vv) let x = t in t | let <x,y>=t(tensore) in t

valori variabili labdax.t coppie valori

if v thent else t unica cosa per i bit

U(v1...vn), U sta nelle gates e misura

Regola di tipo 

derivo statement t ha un tipo tau. ogni variabile viene con un suo tipo

Assunzine che ogni variabile abbia il loro tipo

Controllo di linearita
    Non posso utilizzare una variabile in 2 spazi del trminae
    Cattura il principio di no clonig tipo ineare

Algoritmo type checking
    Analizzo da come viaggiano i dati

i valri sono termini

Tipo di una funzione, do ad un termine un tipo, in Haskell posso fare una funzione con tipo e ogni termine ha il su tipo

tensoriale

il doppio let, non possiamo ocnsiderare una parte sola di una coppia, non si puo far comparire x due volte

nel let dichiaro sempre il tipo

Compiling lambda terms into quantum

Materiale su internet, lambda calcolo lineare o quantistico

vw 2 valori applixazione di una funzione a un argomento

Pattern, algorito doic, dove f non e nota, utilizzo di funzioni di ordine superiore per maggiore modularita

Applicazione come prototipo che deve esser valut applicata ad argomento non sia tra i valori, importante

x funzione senza nome, 


let entangle = lambda q1 : Qbit -> lambda q2 : QBit  -> CX q1 q2
in 
        entangle let q_1 = new Qubit(...) in H q_1 let q_2 = new Qubit(...) in H q_2  


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

