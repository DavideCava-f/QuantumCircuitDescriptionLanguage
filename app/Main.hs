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
import DerivationZipper (termOf)
import qualified Data.Map as Map
import System.Environment (getArgs)

-- Type parsing
pTypeAtom :: Parser Type
pTypeAtom =
      (rWord "bit"  >> return TBit)
  <|> (rWord "qbit" >> return TQbit)
  <|> parens pType

pType :: Parser Type
pType = do
  t1 <- pTypeAtom
  do
      symbol "->"
      t2 <- pType
      return (TFun t1 t2)
   <|> do
      tensor
      t2 <- pType
      return (TPair t1 t2)
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
    _        <- rWord "let"
    (i1, i2) <- angles $ do
        v1 <- identifier
        comma
        v2 <- identifier
        return (v1, v2)
    _        <- equal
    t1       <- termParser
    _        <- rWord "in"
    Decomp i1 i2 t1 <$> termParser

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
                _  <- comma
                t2 <- termParser
                return [t1, t2]
            return (Gate name args)

        "H"    -> oneArgGate name
        "X"    -> oneArgGate name
        "M"    -> oneArgGate name

        _ -> fail $ "Unknown gate: " ++ name

oneArgGate :: String -> Parser Term
oneArgGate name = do
    arg <- parens termParser
    return (Gate name [arg])

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

pairParser :: Parser Value
pairParser = do
    (i1, i2) <- angles $ do
        v1   <- termParser
        _    <- comma
        v2   <- termParser
        return (v1, v2)
    return (Tensor i1 i2)


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
                    pPrint term
                    pPrint ctx
                    case annotate ctx term of
                        Left typeErr -> putStrLn $ "Type/Linearity Error: " ++ typeErr
                        Right (_, typedAST) -> do
  --                          pPrint typedAST
                            let c = startDerivation (Map.fromList ctx) typedAST in
--                                printDerivation c
                                case startMachine c of
                                  Left machineErr -> putStrLn $ "Machine error: " ++ machineErr
                                  Right (final, assocList, finalList) -> do
                                    print final
                                    prettyPrintRootType c
                                    prettyPrintAssocList assocList finalList
                                    let wireNames = [ (lab, wireName p) | (p, lab) <- assocList ++ finalList ]
                                    printAsciiCircuit wireNames final
        [] -> putStrLn "Error: You must specify a filename! (e.g., cabal run -- file.qqdc)"


-- Printing Root Type
getRootType :: TypeDerivation -> Type
getRootType derivation =
  let (_, _, rootType) = rootLabel derivation
  in rootType

formatRootType :: TypeDerivation -> String
formatRootType derivation = "Root Type: " ++ show (getRootType derivation)

prettyPrintRootType :: TypeDerivation -> IO ()
prettyPrintRootType derivation = putStrLn (formatRootType derivation)


gateInputs, gateOutputs :: TransformedGate -> [Label]
gateInputs  (SingleGate _ i _)     = [i]
gateInputs  (FullCNOT i1 _ i2 _)   = [i1, i2]
gateOutputs (SingleGate _ _ o)     = [o]
gateOutputs (FullCNOT _ o1 _ o2)   = [o1, o2]

isIdentityGate :: TransformedGate -> Bool
isIdentityGate (SingleGate "I" _ _) = True
isIdentityGate _                    = False

-- Readable name of the wire starting/ending at a position: the context
-- variable, the name of the `new`, or the L/R path in the conclusion.
wireName :: Pos -> String
wireName (Pos z (Occ f path _)) = case (f, termOf z) of
  (InPrem x, _)         -> x
  (InConcl, TNew _ v _) -> v
  (InConcl, _)          -> show path

-- Arguments: names associated with the initial/final labels (may be empty) and the circuit.
asciiCircuit :: [(Label, String)] -> FinalCircuit -> String
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
    outFor (SingleGate _ _ o) _ = o
    outFor (FullCNOT i1 o1 _ o2) l = if l == i1 then o1 else o2
    chain l = case Map.lookup l consumer of
      Nothing -> ([], l)
      Just gi -> let (gs, end) = chain (outFor (gateAt gi) l) in (gi : gs, end)
    rows = [ (l, gs, end) | l <- initials, let (gs, end) = chain l ]

    -- Row of a gate: the row whose wire crosses it with the given label
    rowOfLabel = Map.fromList
      [ (lab, r) | (r, (l0, gs, _)) <- zip [0 :: Int ..] rows
                 , lab <- l0 : concat [ gateOutputs (gateAt gi) | gi <- gs ] ]
    rowIn lab = Map.findWithDefault (-1) lab rowOfLabel

    cnotSpans = [ (cols Map.! gi, min r1 r2, max r1 r2)
                | (gi, FullCNOT i1 _ i2 _) <- indexed, let r1 = rowIn i1, let r2 = rowIn i2 ]

    -- Column where the wire becomes classical (after an M), if any
    measureCol gs = case [ cols Map.! gi | gi <- gs, SingleGate "M" _ _ <- [gateAt gi] ] of
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
           (SingleGate g _ _ : _)   -> [wire, wire] ++ take 1 g ++ [wire, wire]
           (FullCNOT i1 _ _ _ : _)  -> [wire, wire] ++ (if rowIn i1 == r then "o" else "X") ++ [wire, wire]
           []                       -> plain

    gapCell r c = if any (\(cc, lo, hi) -> cc == c && lo <= r && r < hi) cnotSpans then "  |  " else "     "

    rowLines r (l0, gs, end) =
      let mCol  = measureCol gs
          body  = concatMap (cell gs mCol r) [0 .. nCols - 1]
          line  = padRight leftWidth (nameOf l0) ++ " " ++ body ++ " " ++ nameOf end
          gap   = replicate (leftWidth + 1) ' ' ++ concatMap (gapCell r) [0 .. nCols - 1]
      in if r < length rows - 1 then [line, gap] else [line]

printAsciiCircuit :: [(Label, String)] -> FinalCircuit -> IO ()
printAsciiCircuit names circuit = putStr (asciiCircuit names circuit)