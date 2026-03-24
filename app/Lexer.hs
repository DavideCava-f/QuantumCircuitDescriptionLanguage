module Lexer where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Space Consumer (// e /* */)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

rWord :: String -> Parser String
rWord w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)



-- Wrapper definisco wrapper per lessemi e simboli
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reservedWords :: [String]
reservedWords = ["let", "in", "if", "then", "else", "bit", "qbit"]

-- Parser per identificatori 
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` reservedWords
              then fail $ "La parola riservata '" ++ x ++ "' non può essere un nome"
              else return x
 
 -- Parentesi
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

-- Utils per il parsing
arrow  :: Parser String
arrow  = symbol "->"

tensor :: Parser String
tensor = symbol "⊗" <|> symbol "*" 

lambda :: Parser String
lambda = symbol "λ" <|> symbol "\\" 

dot, colon, equal, comma :: Parser String
dot    = symbol "."
colon  = symbol ":"
equal  = symbol "="
comma  = symbol ","             


lexerTest :: Parser [String]
lexerTest = sc *> many (rWord "let" <|> identifier <|> equal <|> rWord "in" <|> lambda <|> dot) <* eof


