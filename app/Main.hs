module Main where

import Text.Pretty.Simple (pPrint)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Lexer
import TypeTree
import CreateDerivation
import CircuitGraph
import System.Environment (getArgs, getProgName)

-- JOSE:
import Data.List (nub, sort)
import qualified Data.Map as Map

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
      tensor 
      t2 <- pType 
      return (TPair t1 t2)
   <|> return t1) 

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
    t2 <- termParser
    return(Let v tipo t1 t2)

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
    v <- termParser
    rWord "then"
    t1 <- termParser
    rWord "else"
    t2 <- termParser
    return(If v t1 t2)

newParser :: Parser Term
newParser = do
    rWord "new"
    val <- parens integer <|> integer
    if val == 0 || val == 1
        then return (New val)
        else fail "L'argomento di 'new' deve essere 0 oppure 1"

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
    colon
    tipo <- pType
    dot
    t <- termParser
    return(Lambda x tipo t)

pairParser :: Parser Value
pairParser = do
    (i1, i2) <- angles $ do
        v1 <- termParser
        comma
        v2 <- termParser
        return (v1, v2)
    return(Tensor i1 i2)


varParser :: Parser Value
varParser = do
    v <- identifier
    return(Var v)
    


mainParser :: Parser Term
mainParser = sc *> termParser <* eof 

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Argomenti ricevuti: " ++ show args
    case args of
        (filePath:_) -> do
            contenuto <- readFile filePath
            putStrLn $ "FILE: " ++ show contenuto
            case Text.Megaparsec.runParser mainParser "" contenuto of
                Left err -> putStrLn $ "Errore di Sintassi: " ++ show err
                Right ast -> do
--                    pPrint ast
                    let initialCtx = [("q1", TQbit), ("q2", TQbit), ("q3", TQbit)]
                    case annotate initialCtx ast of
                        Left typeErr -> putStrLn $ "Errore di Tipo/Linearità: " ++ typeErr
                        Right (typedAST, remainingCtx) -> do
                            let c = startDerivation typedAST in do
                                let (final, assocList, finalList) = startMachine c
                                printAsciiCircuit final
                                prettyPrintRootType c
                                pPrint c
                            {-let c = startDerivation typedAST in do
--                                printDerivation c
                                let (final, assocList, finalList) = startMachine c
                                print final
                                prettyPrintRootType c
                                prettyPrintAssocList assocList finalList-}
        [] -> putStrLn "Errore: Devi specificare il nome di un file! (es. cabal run -- file.qqdc)"


-- Printing Root Type
getRootType :: TypeDerivation -> Type
getRootType derivation = 
  let (_, _, rootType) = rootLabel derivation
  in rootType

formatRootType :: TypeDerivation -> String
formatRootType derivation = "Root Type: " ++ show (getRootType derivation)

prettyPrintRootType :: TypeDerivation -> IO ()
prettyPrintRootType derivation = putStrLn (formatRootType derivation)

-- 

-- | Converts a FinalCircuit into a visual ASCII diagram with spacer rows
printAsciiCircuit :: [TransformedGate] -> IO ()
printAsciiCircuit gates = do
    let -- 1. Trace dynamic labels to their root physical wires
        buildRoots m (GateI (Lab i) (Lab o)) = Map.insert o (findRoot i m) m
        buildRoots m (SingleGate _ (Lab i) (Lab o)) = Map.insert o (findRoot i m) m
        buildRoots m (FullCNOT (Lab ci) (Lab co) (Lab ti) (Lab to)) = 
            Map.insert to (findRoot ti m) (Map.insert co (findRoot ci m) m)
        findRoot x m = Map.findWithDefault x x m
        rootMap = foldl buildRoots Map.empty gates

        -- 2. Convert raw gates to generic logical operations
        toLogical (GateI _ _) = []
        toLogical (SingleGate name (Lab i) _) = [(name, [findRoot i rootMap])]
        toLogical (FullCNOT (Lab ci) _ (Lab ti) _) = [("CNOT", [findRoot ci rootMap, findRoot ti rootMap])]
        logGates = concatMap toLogical gates

        -- 3. Remap root labels to sequential qubit indices (0, 1, 2...)
        uniqueRoots = sort $ nub $ concatMap snd logGates
        qIndex r = maybe 0 id (lookup r (zip uniqueRoots [0..]))
        mappedGates = map (\(n, qs) -> (n, map qIndex qs)) logGates
        
        numQubits = length uniqueRoots
        
        -- 4. ASCII Drawing Logic with Spacer Rows
        folder lines (name, [q]) = 
            let maxLen = maximum (map length lines)
                -- Pad existing lines with '─' for wires (even) and ' ' for spacers (odd)
                padded = zipWith (\i l -> l ++ replicate (maxLen - length l) (if even i then '─' else ' ')) [0..] lines
                wireIdx = q * 2
                
                updateLine i str
                    | i == wireIdx = str ++ (if name == "I" then "──" else "──[" ++ name ++ "]──")
                    | even i       = str ++ replicate (length name + 4) '─'
                    | otherwise    = str ++ replicate (length name + 4) ' '
                
            in zipWith updateLine [0..] padded

        folder lines ("CNOT", [q1, q2]) = 
            let maxLen = maximum (map length lines)
                padded = zipWith (\i l -> l ++ replicate (maxLen - length l) (if even i then '─' else ' ')) [0..] lines
                minIdx = min q1 q2 * 2
                maxIdx = max q1 q2 * 2
                
                updateLine i str
                    | i == q1 * 2 = str ++ "───●───"
                    | i == q2 * 2 = str ++ "──(X)──"
                    | i > minIdx && i < maxIdx && even i = str ++ "───|───" -- cross-wire
                    | i > minIdx && i < maxIdx && odd i  = str ++ "   |   " -- cross-spacer
                    | even i                             = str ++ "───────" -- empty wire
                    | otherwise                          = str ++ "       " -- empty spacer
                
            in zipWith updateLine [0..] padded
            
        folder lines _ = lines -- Fallback

        -- Generate initial prefixes and blank spacer rows
        prefix i = "q" ++ show i ++ ": "
        maxPref = if numQubits == 0 then 0 else maximum (map (length . prefix) [0..numQubits-1])
        padPref s = s ++ replicate (maxPref - length s) ' '
        
        initialLines = concat [ [padPref (prefix i)] ++ if i < numQubits - 1 then [replicate maxPref ' '] else [] | i <- [0 .. numQubits-1] ]
        
        finalLines = foldl folder initialLines mappedGates

    putStrLn "=============================\n"
    if null logGates 
       then putStrLn "(Empty Circuit)"
       else mapM_ putStrLn finalLines
    putStrLn "=============================\n"


