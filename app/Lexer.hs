module Lexer where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Space Consumer (// and /* */)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

rWord :: String -> Parser String
rWord w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)



-- Wrappers: define wrappers for lexemes and symbols
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

reservedWords :: [String]
reservedWords = ["let", "in", "if", "then", "else", "bit", "qbit", "new"]

-- Parser for identifiers 
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` reservedWords
              then fail $ "La parola riservata '" ++ x ++ "' non può essere un nome"
              else return x
 
 -- Parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

-- Parsing utils
arrow  :: Parser String
arrow  = symbol "->"

tensor :: Parser String
tensor = symbol "⊗" <|> symbol "*" 

lambda :: Parser String
lambda = symbol "λ" <|> symbol "\\" 

turnstile :: Parser String
turnstile = symbol "⊢" <|> symbol "|-"

dot, colon, equal, comma :: Parser String
dot    = symbol "."
colon  = symbol ":"
equal  = symbol "="
comma  = symbol ","             


lexerTest :: Parser [String]
lexerTest = sc *> many (rWord "let" <|> identifier <|> equal <|> rWord "in" <|> lambda <|> dot) <* eof


