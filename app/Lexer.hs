module Lexer where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Definiamo il tipo base del nostro Parser
type Parser = Parsec Void String

-- Space Consumer: ignora spazi e commenti stile C (// e /* */)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

rWord :: String -> Parser String
rWord w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

-- Wrapper per i token: ogni atomo consuma lo spazio che lo segue
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Wrapper per stringhe fisse (simboli)
symbol :: String -> Parser String
symbol = L.symbol sc

reservedWords :: [String]
reservedWords = ["let", "in", "if", "then", "else", "bit", "qbit"]

-- Parser per identificatori (nomi di variabili o funzioni)
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` reservedWords
              then fail $ "La parola riservata '" ++ x ++ "' non può essere un nome"
              else return x
 
 -- Parentesi e delimitatori
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

-- Operatori e punteggiatura
arrow  :: Parser String
arrow  = symbol "->"

tensor :: Parser String
tensor = symbol "⊗" <|> symbol "*" -- Supportiamo entrambi

lambda :: Parser String
lambda = symbol "λ" <|> symbol "\\" -- Carattere corrisponde a \

dot, colon, equal, comma :: Parser String
dot    = symbol "."
colon  = symbol ":"
equal  = symbol "="
comma  = symbol ","             


lexerTest :: Parser [String]
lexerTest = sc *> many (rWord "let" <|> identifier <|> equal <|> rWord "in" <|> lambda <|> dot) <* eof


