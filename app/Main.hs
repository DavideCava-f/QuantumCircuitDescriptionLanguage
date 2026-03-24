module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Lexer

type Name = String

data Type = TBit | TQbit | TFun Type Type | TPair Type Type
  deriving (Eq, Show)


data Term 
  = App Term Term -- H(x) CNOT(x,y) 
  | Let Name Term Term -- let x = q1 in H(x)
  | Decomp Name Name Term Term  -- let <x,y> = t in t
  | If Value Term Term
  | Gate String [Term]          -- Per U(v1...vn) come H, X, CNOT
  | V Value
  deriving (Show)


data Value
  = Var Name
  | Lambda Name Term
  | Pair Term Term
  deriving (Show)
-- Parsing Dei tipi
pTypeAtom :: Parser Type
pTypeAtom = 
      (rWord "bit"  >> return TBit)
  <|> (rWord "qbit" >> return TQbit)
  <|> parens pType  

pType :: Parser Type
pType = do
  t1 <- pTypeAtom
  (do 
      symbol "->" 
      t2 <- pType 
      return (TFun t1 t2)
   <|> do 
      symbol "⊗" -- o "*" 
      t2 <- pType 
      return (TPair t1 t2)
   <|> return t1) 

--------
---- Term Parser

termParser :: Parser Term
termParser = try letParser <|> try decompParser <|> try ifParser <|> try gateParser <|> applicationParser

letParser :: Parser Term
letParser = do
    rWord "let"
    v <- identifier
    equal 
    t1 <- termParser  
    rWord "in"
    t2 <- termParser
    return(Let v t1 t2)

decompParser :: Parser Term
decompParser = do
    rWord "let"
    (i1, i2) <- angles $ do
        v1 <- identifier
        comma
        v2 <- identifier
        return (v1, v2)
    equal
    t1 <- termParser
    rWord "in"
    t2 <- termParser
    return(Decomp i1 i2 t1 t2)

ifParser :: Parser Term
ifParser = do
    rWord "if"
    v <- valueParser
    rWord "then"
    t1 <- termParser
    rWord "else"
    t2 <- termParser
    return(If v t1 t2)

validGates :: Parser String
validGates = choice [ string "H"
                    , string "X"
                    , string "CNOT"
                    , string "M"
                    ] <* sc

gateParser :: Parser Term
gateParser = do
    name <- identifier 
    case name of
        "CNOT" -> do
            args <- parens $ do
                t1 <- termParser
                comma
                t2 <- termParser
                return [t1, t2]
            return (Gate name args)
            
        "H" -> oneArgGate name
        "X" -> oneArgGate name
        "M" -> oneArgGate name
        
        _ -> fail $ "Unknown gate: " ++ name

oneArgGate :: String -> Parser Term
oneArgGate name = do
    arg <- parens termParser 
    return (Gate name [arg])

----

atomParser :: Parser Term
atomParser = (V <$> valueParser)
    <|> parens termParser

applicationParser:: Parser Term
applicationParser = do
  atoms <- some atomParser      --[a1, a2, a3...]
  return (foldl1 App atoms) -- App (App a1 a2) a3

--------

valueParser :: Parser Value
valueParser = try pairParser <|> try  lambdaParser <|> varParser 


lambdaParser :: Parser Value
lambdaParser = do
    lambda
    x <- identifier
    dot
    t <- termParser
    return(Lambda x t)

pairParser :: Parser Value
pairParser = do
    (i1, i2) <- angles $ do
        v1 <- termParser
        comma
        v2 <- termParser
        return (v1, v2)
    return(Pair i1 i2)


varParser :: Parser Value
varParser = do
    v <- identifier
    return(Var v)
    


mainParser :: Parser Term
mainParser = sc *> termParser <* eof 


main :: IO ()
main = do
    let try = "let sup = \\f.\\x. let y = H(x) in let z = f y in H(z) in sup q" 
    putStrLn("Prova:" ++ try)
    input <- getLine
    case Text.Megaparsec.runParser mainParser "" input of
        Left err  -> putStrLn (errorBundlePretty err) 
        Right res -> print res

