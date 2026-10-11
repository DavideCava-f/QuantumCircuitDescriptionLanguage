module Main where

import Text.Pretty.Simple (pPrint)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer()
import Data.Void()
import Lexer
import TypeTree
import CreateDerivation
import CircuitGraph
import DerivationZipper (termOf, fromTree)
import qualified Data.Map as Map
import System.Environment (getArgs)

-- Type parsing
pTypeAtom :: Parser Type
pTypeAtom =
      (rWord "bit"  >> return TBit)
  <|> (rWord "qbit" >> return TQbit)
  <|> parens pType

-- A1 * ... * An -> B: the tensor binds tighter than the arrow, which associates
-- to the right. A tensor of n >= 2 atoms is one n-ary TPair.
pType :: Parser Type
pType = do
  atoms <- pTypeAtom `sepBy1` tensor
  let t1 = case atoms of
        [t] -> t
        ts  -> TPair ts
  do
      _ <- symbol "->"
      t2 <- pType
      return (TFun t1 t2)
   <|> return t1

--------
---- Term Parser

termParser :: Parser Term
termParser = try letParser <|> try decompParser <|> try ifParser <|> try newParser <|> try gateParser <|> applicationParser



letParser :: Parser Term
letParser = do
    rWord "let"
    v <- identifier
    colon
    tipo <- pType
    equal
    t1 <- termParser
    rWord "in"
    Let v tipo t1 <$> termParser

decompParser :: Parser Term
decompParser = do
    _     <- rWord "let"
    names <- angles (identifier `sepBy1` comma)
    _     <- equal
    t1    <- termParser
    _     <- rWord "in"
    Decomp names t1 <$> termParser

ifParser :: Parser Term
ifParser = do
    _  <-rWord "if"
    v  <- termParser
    _  <- rWord "then"
    t1 <- termParser
    _  <- rWord "else"
    If v t1 <$> termParser

newParser :: Parser Term
newParser = do
    rWord "new"
    val <- parens integer <|> integer
    if val == 0 || val == 1
        then return (New val)
        else fail "The argument for 'new' must be 0 or 1"

-- Gate application NAME(t1, ..., tn): the name is one or more upper-case
-- letters followed by '('. Which names are valid gates, and with how many
-- arguments, is decided by the type checker (checkGate).
gateName :: Parser String
gateName = try (lexeme (some upperChar <* notFollowedBy (alphaNumChar <|> char '_')) <* lookAhead (char '('))

gateParser :: Parser Term
gateParser = do
    name <- gateName
    args <- parens (termParser `sepBy1` comma)
    return (Gate name args)

----

atomParser :: Parser Term
atomParser = V <$> valueParser
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
    _    <- lambda
    x    <- identifier
    _    <- colon
    tipo <- pType
    _    <- dot
    Lambda x tipo <$> termParser

-- Tuple <t1, ..., tn> with n >= 2
pairParser :: Parser Value
pairParser = do
    ts <- angles (termParser `sepBy1` comma)
    case ts of
        [_] -> fail "A tuple needs at least two components"
        _   -> return (Tensor ts)


varParser :: Parser Value
varParser = do
    Var <$> identifier



-- Context Parser: x1 : A1, ..., xn : An (possibly empty)
bindingParser :: Parser (Name, Type)
bindingParser = do
    x <- identifier
    colon
    tipo <- pType
    return (x, tipo)

contextParser :: Parser Context
contextParser = do
    ctx <- bindingParser `sepBy` comma
    case [ x | (i, (x, _)) <- zip [0 :: Int ..] ctx, x `elem` map fst (take i ctx) ] of
        []      -> return ctx
        (x : _) -> fail $ "Variable '" ++ x ++ "' is declared twice in the context"

-- Γ |- M, if |- M or just M context is considered empty
sequentParser :: Parser (Context, Term)
sequentParser = do
    hasTurnstile <- option False (True <$ try (lookAhead (skipManyTill anySingle turnstile)))
    ctx <- if hasTurnstile then contextParser <* turnstile else return []
    t <- termParser
    return (ctx, t)

mainParser :: Parser (Context, Term)
mainParser = sc *> sequentParser <* eof

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Arguments received " ++ show args
    case args of
        (filePath:_) -> do
            contenuto <- readFile filePath
            putStrLn $ "FILE: " ++ show contenuto
            case Text.Megaparsec.runParser mainParser "" contenuto of
                Left err -> putStrLn $ "Syntax Error: " ++ show err
                Right (ctx, term) -> do
                    case annotate ctx term of
                        Left typeErr -> putStrLn $ "Type/Linearity Error: " ++ typeErr
                        Right (_, typedAST) -> do
                            pPrint (startDerivation (Map.fromList ctx) typedAST)
                            let c = startDerivation (Map.fromList ctx) typedAST
                            prettyPrintPositions "Initials positions: " (findInitials (fromTree c))
                            case startMachine c of
                              Left machineErr -> putStrLn $ "Machine error: " ++ machineErr
                              Right (final, assocList, finalList) -> do
                                --print final
                                --prettyPrintRootType c
                                --prettyPrintAssocList assocList finalList
                                let wireNames = [ (lab, wireName p) | (p, lab) <- assocList ++ finalList ]
                                printAsciiCircuit wireNames final
        [] -> putStrLn "Error: You must specify a filename! (e.g., cabal run -- examples/file.qqdc)"


-- Printing Root Type
getRootType :: TypeDerivation -> Type
getRootType derivation =
  let (_, _, rootType) = rootLabel derivation
  in rootType

formatRootType :: TypeDerivation -> String
formatRootType derivation = "Root Type: " ++ show (getRootType derivation)

prettyPrintRootType :: TypeDerivation -> IO ()
prettyPrintRootType derivation = putStrLn (formatRootType derivation)


gateInputs, gateOutputs :: Gate -> [Label]
gateInputs  (Wire _ i _)     = [i]
gateInputs  (MultiGate _ is _)   = is
gateOutputs (Wire _ _ o)     = [o]
gateOutputs (MultiGate _ _ os)   = os

isIdentityGate :: Gate -> Bool
isIdentityGate (Wire "I" _ _) = True
isIdentityGate _                    = False

-- Readable name of the wire starting/ending at a position: the context
-- variable, the name of the `new`, or the L/R path in the conclusion.
wireName :: Pos -> String
wireName (Pos z (Occ f path _)) = case (f, termOf z) of
  (InPrem x, _)         -> x
  (InConcl, TNew _ v _) -> v
  (InConcl, _)          -> show path

-- Arguments: names associated with the initial/final labels (may be empty) and the circuit.
asciiCircuit :: [(Label, String)] -> Circuit -> String
asciiCircuit names circuit
  | null circuit = "(empty circuit)"
  | otherwise    = unlines (concat (zipWith rowLines [0 ..] rows))
  where
    indexed  = zip [0 :: Int ..] circuit
    gateAt i = circuit !! i
    producer = Map.fromList [ (o, gi) | (gi, g) <- indexed, o <- gateOutputs g ]
    consumer = Map.fromList [ (i, gi) | (gi, g) <- indexed, i <- gateInputs g ]

    labInt (Lab n) = n
    initials = map snd (Map.toAscList (Map.fromList
                 [ (labInt l, l) | g <- circuit, l <- gateInputs g, not (Map.member l producer) ]))

    -- Column of each gate (lazy map: the circuit is acyclic)
    cols :: Map.Map Int Int
    cols = Map.fromList [ (gi, colOf gi g) | (gi, g) <- indexed ]
    colOf _ g =
      let base = maximum ((-1) : [ cols Map.! p | i <- gateInputs g, Just p <- [Map.lookup i producer] ])
      in if isIdentityGate g then base else base + 1
    -- one extra final column, so that a wire with no gates is still drawn
    nCols = 2 + maximum ((-1) : [ c | (gi, c) <- Map.toList cols, not (isIdentityGate (gateAt gi)) ])

    -- Wire: (initial label, gates crossed, final label)
    outFor (Wire _ _ o) _ = o
    outFor (MultiGate _ is os) l = maybe l (os !!) (lookup l (zip is [0 ..]))
    chain l = case Map.lookup l consumer of
      Nothing -> ([], l)
      Just gi -> let (gs, end) = chain (outFor (gateAt gi) l) in (gi : gs, end)
    rows = [ (l, gs, end) | l <- initials, let (gs, end) = chain l ]

    -- Row of a gate: the row whose wire crosses it with the given label
    rowOfLabel = Map.fromList
      [ (lab, r) | (r, (l0, gs, _)) <- zip [0 :: Int ..] rows
                 , lab <- l0 : concat [ gateOutputs (gateAt gi) | gi <- gs ] ]
    rowIn lab = Map.findWithDefault (-1) lab rowOfLabel

    cnotSpans = [ (cols Map.! gi, minimum rs, maximum rs)
                | (gi, MultiGate _ is _) <- indexed, let rs = map rowIn is ]

    -- Column where the wire becomes classical (after an M), if any
    measureCol gs = case [ cols Map.! gi | gi <- gs, Wire "M" _ _ <- [gateAt gi] ] of
      []      -> Nothing
      (c : _) -> Just c

    nameOf l = maybe "" (++ " ") (lookup l names) ++ show l
    leftWidth = maximum (0 : [ length (nameOf l) | (l, _, _) <- rows ])
    padRight n s = s ++ replicate (n - length s) ' '

    cell :: [Int] -> Maybe Int -> Int -> Int -> String
    cell gs mCol r c =
      let wire = if maybe False (< c) mCol then '=' else '-'
          plain = replicate 5 wire
          here = [ gateAt gi | gi <- gs, cols Map.! gi == c, not (isIdentityGate (gateAt gi)) ]
      in case here of
           (Wire g _ _ : _)   -> [wire, wire] ++ take 1 g ++ [wire, wire]
           -- controls (all inputs but the last) as 'o', the target as 'X'
           (MultiGate _ is _ : _) -> [wire, wire] ++ (if rowIn (last is) == r then "X" else "o") ++ [wire, wire]
           []                       -> plain

    gapCell r c = if any (\(cc, lo, hi) -> cc == c && lo <= r && r < hi) cnotSpans then "  |  " else "     "

    rowLines r (l0, gs, end) =
      let mCol  = measureCol gs
          body  = concatMap (cell gs mCol r) [0 .. nCols - 1]
          line  = padRight leftWidth (nameOf l0) ++ " " ++ body ++ " " ++ nameOf end
          gap   = replicate (leftWidth + 1) ' ' ++ concatMap (gapCell r) [0 .. nCols - 1]
      in if r < length rows - 1 then [line, gap] else [line]

printAsciiCircuit :: [(Label, String)] -> Circuit -> IO ()
printAsciiCircuit names circuit = putStr (asciiCircuit names circuit)