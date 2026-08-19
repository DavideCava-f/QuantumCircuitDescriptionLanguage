module CircuitGraph where

--import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeTree (Type(..),Term(..),TypedTerm(..),Value(..),TypedValue(..))
import CreateDerivation (Prem(..),Concl(..),TypeDerivation(..),Tree(..))
import Debug.Trace (trace, traceShowId)

type Address = Map.Map String String
data PosInSeq = Concl | Prem String Int deriving (Show,Eq)
type PosInPi = Int 
emptyAddress :: Address
emptyAddress = Map.empty

data Rule = TLAMBDA String | TGATE String | TTENSOR | TVAR | TAPP | TDECOMP String String | TLET String deriving (Show)
data Position = L | R deriving (Show, Eq)
data Polarity = P | N deriving (Show, Eq)
type Id = (TypedTerm, Polarity, [Position], PosInSeq, [PosInPi])-- deriving (Show)
data Label = Lab Int deriving (Show,Eq)
type Token = (Id, Label, Address) 
type DATA = [Id]
type Cable = String --Label H Cable | Label Cable | F
type Circuit = [Cable]

data TokenState = TokenState
  { tokens   :: [Token]
  , lastLabel :: Int
  } deriving (Show)
emptyTokenState :: TokenState
emptyTokenState = TokenState [] 0


inferRule :: Token -> DATA -> Rule
inferRule token@((term, pol, pos, seq, pi), lab, addr) allData =
  let 

    termToInspect = if pol == P
                      then let fatherId = head (filterByPathPi (dropLast pi) allData)
                           in getTerm fatherId
                      else term
  in 

    case termToInspect of
      TV v _ -> case v of
        TVar _ _          -> TVAR
        TLambda x _ _ _   -> TLAMBDA x
        TTensor _ _ _   -> TTENSOR 
      TDecomp z x _ _ _ -> TDECOMP z x
      TApp _ _ _ -> TAPP
      TLet x _ _ _ _ -> TLET x
      TGate g _ _ -> TGATE g
      _                    -> error "Termine non riconosciuto"

applyGate :: String -> Token -> DATA -> Token
applyGate g token@((term, pol, pos, seq, pi), lab, addr) allData =

  case seq of

    -- ==========================================
    -- CASO 1: TOKEN IN PREMESSA (Prem z _)
    -- ==========================================
    Prem z idx ->
      case pol of
        -- POLARITÀ NEGATIVA (N):
        -- Trova tra i figli (0 o 1) quello che possiede la premessa 'z'
        N ->
          let targetData = filterByPathPi (pi ++ [1]) allData
              premiseData = getPremiseN z targetData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

        -- POLARITÀ POSITIVA (P):
        -- Risali al padre (dropLast pi) e cerca la premessa 'z'
        P ->
          let parentPi    = dropLast pi
              parentData  = filterByPathPi parentPi allData
              premiseData = getPremiseN z parentData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

    Concl ->
        case pol of
            P ->
                let whichSon = last pi
                    parentId = dropLast pi
                in
                    if whichSon == 1
                        then 
                            let matchedId = traceShowId (filterByPathPi (parentId ++ [0]) . filterConcl . filterByPosLR (L : pos) $ allData)
                            in
                                (head matchedId, lab, addr)
                        else
                             let matchedId = traceShowId (filterByPathPi (parentId) . filterConcl . filterByPosLR (tail pos) $ allData)
                             in 
                                (head matchedId, lab, addr)
            N -> 
                 let matchedId = traceShowId (filterByPathPi (pi) . filterConcl . filterByPosLR (R : (tail pos)) $ allData)
                 in 
                    (head matchedId, lab, addr)

applyApp :: Token -> DATA -> Token
applyApp token@((term, pol, pos, seq, pi), lab, addr) allData =

  case seq of

    -- ==========================================
    -- CASO 1: TOKEN IN PREMESSA (Prem z _)
    -- ==========================================
    Prem z idx ->
      case pol of
        -- POLARITÀ NEGATIVA (N):
        -- Trova tra i figli (0 o 1) quello che possiede la premessa 'z'
        N ->
          let dataChild0 = filterByPathPi (pi ++ [0]) allData
              dataChild1 = filterByPathPi (pi ++ [1]) allData
              
              targetData = if hasPremise z dataChild0 
                             then dataChild0 
                             else dataChild1
                             
              premiseData = getPremiseN z targetData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

        -- POLARITÀ POSITIVA (P):
        -- Risali al padre (dropLast pi) e cerca la premessa 'z'
        P ->
          let parentPi    = dropLast pi
              parentData  = filterByPathPi parentPi allData
              premiseData = getPremiseN z parentData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

    Concl ->
        case pol of
            N ->
                let matchedId = traceShowId (filterByPathPi (pi ++ [0]) . filterConcl . filterByPosLR (R : pos) $ allData) 
                in 
                    (head matchedId, lab, addr)
            P ->
                let whichSon = last pi
                    parentId = dropLast pi
                in
                    if whichSon == 0
                        then 
                            case pos of
                                (p:ps) -> case p of
                                    R -> let matchedId = traceShowId (filterByPathPi (parentId) . filterConcl . filterByPosLR (ps) $ allData)
                                         in
                                            (head matchedId, lab, addr)
                                    L -> let matchedId = traceShowId (filterByPathPi (parentId ++ [1]) . filterConcl . filterByPosLR (ps) $ allData)
                                         in
                                            (head matchedId, lab, addr)
                                _ -> error "Errore Applicazione, non puo essere vuoto il pos del primo figlio"
                        else

                             let matchedId = traceShowId (filterByPathPi (parentId ++ [0]) . filterConcl . filterByPosLR (L : pos) $ allData)
                             in 
                                (head matchedId, lab, addr)

applyVar :: Token -> DATA -> Token
applyVar token@((term, pol, pos, seq, pi), lab, addr) allData =
  case pol of
    N -> 
        case seq of

            Concl ->
                let matchedId = traceShowId (filterByPathPi pi . filterPremises . filterByPosLR pos $ allData)
                in 
                    (head matchedId, lab, addr)
            Prem _ 0 ->
                let matchedId = traceShowId (filterByPathPi pi . filterConcl . filterByPosLR pos $ allData)
                in 
                    (head matchedId, lab, addr)

applyTensor :: Token -> DATA -> Token
applyTensor token@((term, pol, pos, seq, pi), lab, addr) allData =
  case pol of
    N -> case seq of
        -- Connetti a elemento negativo che ha come nome quello in seq, trovare quindi il figlio corrispondente (Si presuppone che siano tutti nella premessa i qbit negativi di tensor)
       Prem y _ ->
         let -- Recuperiamo i sottoalberi per il figlio 0 e il figlio 1
            dataChild0 = filterByPathPi (pi ++ [0]) allData
            dataChild1 = filterByPathPi (pi ++ [1]) allData
            
            -- Scegliamo i dati del figlio che possiede la premessa con nome 'y'
            targetData = if hasPremise y dataChild0
                            then dataChild0
                            else dataChild1
                           
            premiseData = getPremiseN y targetData
            matchedId   = traceShowId (filterByPosLR pos premiseData)
         in (head matchedId, lab, addr)

       Concl -> error "I qubit negativi di Tensor si presuppongono essere nella premessa"

    
    P -> case seq of
        -- Connetti a elemento negativo che ha come nome quello in seq, trovare quindi il figlio corrispondente (Si presuppone che siano tutti nella premessa i qbit negativi di tensor)
       Prem y _ ->
         let -- Recuperiamo i sottoalberi per il figlio 0 e il figlio 1
            dataFather = filterByPathPi (dropLast pi) allData
                           
            premiseData = getPremiseN y dataFather
            matchedId   = traceShowId (filterByPosLR pos premiseData)
         in (head matchedId, lab, addr)

       Concl -> 
         let 
             parentPi  = dropLast pi
             lastIndex = last pi  -- Può essere 0 oppure 1
            
            
             lrPrefix  = if lastIndex == 0 then L else R
            
           
             parentData = filterByPathPi parentPi allData
             conclData  = filterConcl parentData
            
          
             matchedId  = traceShowId (filterByPosLR (lrPrefix : pos) conclData)
        in (head matchedId, lab, addr) -- Ho messo prefisso ma deve essere sempre vuoto [] a questo punto

applyDecomp :: String -> String -> Token -> DATA -> Token
applyDecomp x y token@((term, pol, pos, seq, pi), lab, addr) allData =
  case seq of

    -- ==========================================
    -- CASO 1: TOKEN IN PREMESSA (Prem z _)
    -- ==========================================
    Prem z idx ->
      case pol of
        -- POLARITÀ NEGATIVA (N):
        -- Trova tra i figli (0 o 1) quello che possiede la premessa 'z'
        N ->
          let dataChild0 = filterByPathPi (pi ++ [0]) allData
              dataChild1 = filterByPathPi (pi ++ [1]) allData
              
              targetData = if hasPremise z dataChild0 
                             then dataChild0 
                             else dataChild1
                             
              premiseData = getPremiseN z targetData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

        -- POLARITÀ POSITIVA (P):
        -- Risali al padre (dropLast pi) e cerca la premessa 'z'
        P ->
          let parentPi    = dropLast pi
              parentData  = filterByPathPi parentPi allData
              premiseData = getPremiseN z parentData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

    -- ==========================================
    -- CASO 2: TOKEN IN CONCLUSIONE (Concl)
    -- ==========================================
    Concl ->
      case pol of
        -- POLARITÀ NEGATIVA (N):
        -- Vai nella conclusione del figlio 1 (pi ++ [1])
        N ->
          let child1Data = filterByPathPi (pi ++ [1]) allData
              conclData  = filterConcl child1Data
              matchedId  = traceShowId (filterByPosLR pos conclData)
          in (head matchedId, lab, addr)

        -- POLARITÀ POSITIVA (P):
        P ->
          let lastIndex = last pi
              parentPi  = dropLast pi
          in if lastIndex == 1
               -- Se siamo nel Figlio 1: Vai nella conclusione del padre
               then let parentData = filterByPathPi parentPi allData
                        conclData  = filterConcl parentData
                        matchedId  = traceShowId (filterByPosLR pos conclData)
                    in (head matchedId, lab, addr)

               -- Se siamo nel Figlio 0: Vai al corrispondente nel Figlio 1
               else let sibling1Pi = parentPi ++ [1]
                        siblingData = filterByPathPi sibling1Pi allData
                    in case pos of
                         -- Se la posizione inizia con L -> premessa della variabile x
                         (L : ps) ->
                           let premiseData = getPremiseN x siblingData
                               matchedId   = traceShowId (filterByPosLR ps premiseData)
                           in (head matchedId, lab, addr)

                         -- Se la posizione inizia con R -> premessa della variabile y
                         (R : ps) ->
                           let premiseData = getPremiseN y siblingData
                               matchedId   = traceShowId (filterByPosLR ps premiseData)
                           in (head matchedId, lab, addr)

                         [] -> error "Posizione LR vuota per il figlio 0 in Concl (P)"

applyLambda :: String -> Token -> DATA -> Token
applyLambda x token@((term, pol, pos, seq, pi), lab, addr) allData =
  case seq of

    -- 1. CASO IN GAMMA: Premessa con nome 'y' diverso da 'x'
    Prem y idx | y /= x -> 
      case pol of
        N -> 
          let usefulData  = filterByPathPi (pi ++ [0]) allData
              premiseData = getPremiseN y usefulData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

        P -> 
          let usefulData  = filterByPathPi (dropLast pi) allData
              premiseData = getPremiseN y usefulData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

    -- 2. CASO NON IN GAMMA: Ramo di fallback (quando seq è Prem x idx oppure Concl)
    _ -> 
      case pol of
        N -> 
          case pos of
            (p : ps) -> 
              let usefulData = filterByPathPi (pi ++ [0]) allData
              in case p of
                L -> 
                  let premiseData = getPremiseN x usefulData
                      matchedId   = traceShowId (filterByPosLR ps premiseData)
                  in (head matchedId, lab, addr)

                R -> 
                  let conclData = filterConcl usefulData
                      matchedId = traceShowId (filterByPosLR ps conclData)
                  in (head matchedId, lab, addr)

            [] -> error "Posizione LR vuota"

        P -> 
          let usefulData = filterByPathPi (dropLast pi) allData
          in case seq of
            Concl -> 
              let matchedId = traceShowId (filterConcl . filterByPosLR (R : pos) $ usefulData)
              in (head matchedId, lab, addr)

            Prem _ _ -> 
              let matchedId = traceShowId (filterConcl . filterByPosLR (L : pos) $ usefulData)
              in (head matchedId, lab, addr)

applyLet :: String -> Token -> DATA -> Token
applyLet x token@((term, pol, pos, seq, pi), lab, addr) allData =
  case seq of

    -- 1. CASO IN GAMMA: Premessa con nome 'y' diverso da 'x'
    Prem y idx | y /= x -> 
      case pol of
        N -> 
          let dataChild0 = filterByPathPi (pi ++ [0]) allData
              dataChild1 = filterByPathPi (pi ++ [1]) allData
              
              targetData = if hasPremise y dataChild0 
                             then dataChild0 
                             else dataChild1
                             
              premiseData = getPremiseN y targetData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

        P -> 
          let usefulData  = filterByPathPi (dropLast pi) allData
              premiseData = getPremiseN y usefulData
              matchedId   = traceShowId (filterByPosLR pos premiseData)
          in (head matchedId, lab, addr)

    -- 2. CASO NON IN GAMMA: Prem x A2+
    Prem y _ | x == y -> 
      case pol of
        P -> 
          let parentId = dropLast pi
              usefulData = traceShowId (filterByPathPi (parentId ++ [0]) . filterConcl . filterByPosLR pos $ allData)
          in (head usefulData, lab, addr)

    Concl ->
      case pol of
        P -> 
            let whichSon = last pi
                parentId = dropLast pi
            in 
                if whichSon == 0
                    then 
                         let matchedId = traceShowId (filterByPathPi (parentId ++ [1]) . getPremiseN x . filterByPosLR (pos) $ allData)
                         in 
                            (head matchedId, lab, addr)
                    else

                         let matchedId = traceShowId (filterByPathPi (parentId) . filterConcl . filterByPosLR (pos) $ allData)
                         in 
                            (head matchedId, lab, addr)
        N ->
            let matchedId = traceShowId (filterByPathPi (pi ++ [1]) . filterConcl . filterByPosLR (pos) $ allData)
            in
                (head matchedId, lab, addr)


applyRule :: Token -> Rule -> DATA -> Token
applyRule tok rule allData = case rule of
    TLAMBDA x -> applyLambda x tok allData
    TVAR -> applyVar tok allData
    TAPP -> applyApp tok allData
    TTENSOR -> applyTensor tok allData
    TDECOMP x y -> applyDecomp x y tok allData
    TLET x -> applyLet x tok allData
    TGATE g -> applyGate g tok allData 

stopCond :: Token -> Bool
stopCond ((_, pol, _, _, pi), _, _) = pol == P && null pi

upgradeCircuit :: Label -> Cable
upgradeCircuit lab = "I"

travel :: Token -> DATA -> Cable
travel tok@((term, pol, pos, seq, pi), lab, addr) allData =
  let rule   = inferRule tok allData
      newTok = applyRule tok rule allData
  in 
    if stopCond newTok 
      then upgradeCircuit lab
      else travel newTok allData   


--Running

runMachine :: TokenState -> DATA -> Circuit
runMachine state allData = map (\t -> travel t allData) (tokens state)



-- Tokens


addTokensFromData :: DATA -> TokenState -> TokenState
addTokensFromData dataList (TokenState currentTokens lastIdx) =
  let 
    -- Genera i nuovi token partendo da (lastIdx + 1)
    newTokens = zipWith (\id' idx -> (id', Lab idx, emptyAddress)) dataList [lastIdx + 1 ..]
    
    -- L'ultimo indice diventa lastIdx + elementi aggiunti
    updatedLastIdx = lastIdx + length dataList
  in 
    TokenState (currentTokens ++ newTokens) updatedLastIdx




--Indentify QBITS

startMachine :: TypeDerivation -> DATA
startMachine derivation =
    let
        allData = extractDataRecursive derivation []
    in
        let 
            initials = findInitials allData
        in 
            allData
            
{--
            let 
                activeTokens = addTokensFromData initials (emptyTokenState)
            in
                runMachine activeTokens allData
            
--}


extractDataRecursive :: TypeDerivation -> [PosInPi] -> DATA
extractDataRecursive (Node concl forest) pathPi =
    let 
        -- 1. Estraiamo DATA e NDATA dal nodo corrente (usando il suo pathPi)
        currentData = processJudgment concl pathPi
        
        processForest :: [TypeDerivation] -> Int -> DATA
        processForest [] _ = []
        processForest (child : cs) idx =
            let childD = extractDataRecursive child (pathPi ++ [idx])
                restD  = processForest cs (idx + 1)
            in childD ++ restD

        subData = processForest forest 0
            
    in
        -- 4. Uniamo tutto in un unico grande insieme DATA e NDATA
        currentData ++ subData

processJudgment :: Concl -> [PosInPi] -> DATA
processJudgment (prems, conclTerm, typ) pathPi = 
  let
    -- Launch 1: Processa tutte le premesse (Prem)
    premData = processPremises prems conclTerm pathPi
    
    -- Launch 2: Processa la conclusione (Type)
    conclData = processConcl conclTerm typ pathPi
  in
    -- Unisce i risultati di entrambi
    premData ++ conclData

processPremises :: Prem -> TypedTerm -> [PosInPi] -> DATA
processPremises premMap typedTerm pathPi =
    processList (Map.toList premMap) 0
  where
    -- Funzione ausiliaria che scorre la lista di coppie (NomeVariabile, Type)
    processList :: [(String, Type)] -> Int -> DATA
    processList [] _ = []
    processList ((name, typ) : rest) premIdx =
      let 
        -- 1. Estrae DATA e NDATA per la premessa corrente
        currentD = inspectPremiseType [] True name typ premIdx
        
        -- 2. Ricorsione sulle premesse rimanenti con indice incrementato
        restD       = processList rest (premIdx + 1)
      in 
        -- 3. Unisce i risultati direttamente in una singola coppia
        currentD ++ restD

    inspectPremiseType :: [Position] -> Bool -> String -> Type -> Int -> DATA
    inspectPremiseType lrPath isPositive name TQbit premIdx =
      if isPositive
        then 
            let currentId = (typedTerm, N, lrPath, Prem name premIdx, pathPi) in [currentId]
        else
            let currentId = (typedTerm, P, lrPath, Prem name premIdx, pathPi) in [currentId]


    inspectPremiseType lrPath isPositive name (TFun t1 t2) premIdx =
      let d1 = inspectPremiseType (lrPath ++ [L]) (not isPositive) name t1 premIdx
          d2 = inspectPremiseType (lrPath ++ [R]) isPositive name t2 premIdx
      in (d1 ++ d2)

    inspectPremiseType lrPath isPositive name (TPair t1 t2) premIdx =
      let d1 = inspectPremiseType (lrPath ++ [L]) isPositive name t1 premIdx
          d2 = inspectPremiseType (lrPath ++ [R]) isPositive name t2 premIdx
      in d1 ++ d2


processConcl :: TypedTerm -> Type -> [PosInPi] -> DATA
processConcl typedTerm typ pathPi = inspectType [] True typ
  where
    inspectType :: [Position] -> Bool -> Type -> DATA
    
    inspectType lrPath isPositive TQbit =
      if isPositive
        then 
            let currentId = (typedTerm, P, lrPath, Concl, pathPi) in [currentId]
        else
            let currentId = (typedTerm, N, lrPath, Concl, pathPi) in [currentId]

    inspectType lrPath isPositive (TFun t1 t2) =
      let d1 = inspectType (lrPath ++ [L]) (not isPositive) t1
          d2 = inspectType (lrPath ++ [R]) isPositive t2
      in d1 ++ d2

    inspectType lrPath isPositive (TPair t1 t2) =
      let d1 = inspectType (lrPath ++ [L]) isPositive t1
          d2 = inspectType (lrPath ++ [R]) isPositive t2
      in d1 ++ d2

--Utils
dropLast :: [a] -> [a]
dropLast []       = []
dropLast [_]      = []  
dropLast (x:xs)   = x : dropLast xs

getTerm :: Id -> TypedTerm
getTerm (term, _, _, _, _) = term

hasPremise :: String -> DATA -> Bool
hasPremise y d = not (null (getPremiseN y d))

getVarName :: TypedTerm -> String
getVarName (TV (TVar name _) _) = name
getVarName _                     = error "Atteso un TVar all'interno del termine da decomporre"
----

findInitials :: DATA -> DATA
findInitials = filterByPathPi [] . filterByPolarity N

findFirstLevel :: DATA -> DATA
findFirstLevel = filterByPathPi [0] . filterByPolarity P

filterByPathPi :: [PosInPi] -> [Id] -> [Id]
filterByPathPi targetPath = filter (\( _, _, _, _, pathPi) -> pathPi == targetPath)

filterByPolarity :: Polarity -> [Id] -> [Id]
filterByPolarity targetPol = filter (\( _, pol, _, _, _) -> pol == targetPol)

filterBySeq :: PosInSeq -> [Id] -> [Id]
filterBySeq targetSeq = filter (\( _, _, _, seq, _) -> seq == targetSeq)

filterConcl :: [Id] -> [Id]
filterConcl = filterBySeq Concl

filterByPosLR :: [Position] -> DATA -> DATA
filterByPosLR targetLR = filter (\(_, _, lrPath, _, _) -> lrPath == targetLR)

filterPremises :: [Id] -> [Id]
filterPremises = filter (\( _, _, _, seq, _) -> isPremise seq)
  where
    isPremise (Prem _ _) = True
    isPremise Concl       = False

getPremiseN :: String -> DATA -> DATA
getPremiseN targetName = filter isTargetPrem
  where
    isTargetPrem (_, _, _, Prem x _, _) = x == targetName
    isTargetPrem _                      = False
--Pretty

prettyId :: Id -> String
prettyId (typedTerm, pol, lrPath, posSeq, pathPi) =
  let
    -- Formattazione della posizione nella sequenza (Concl o Prem n)
    seqStr = case posSeq of
      Concl      -> "CONCL "
      Prem x n  -> "PREM " ++ show x ++ show n

    -- Polarità come simbolo (+) o (-)
    polStr = case pol of
      P -> "(+)"
      N -> "(-)"

    -- Cammino L/R (es. [L, R] -> "L.R", [] -> "ε")
    lrStr = if null lrPath 
              then "ε" 
              else foldr1 (\a b -> a ++ "." ++ b) (map show lrPath)

    -- Cammino nell'albero Pi (es. [0, 1] -> "π[0.1]")
    piStr = "π" ++ show pathPi

    -- Estraiamo solo il tipo o il termine per brevità (o usiamo `show typedTerm`)
    termStr = show typedTerm 
  in
    concat [ "[", seqStr, " | ", piStr, "] "
           , polStr, " LR: ", lrStr
           , "  ==>  ", termStr
           ]

-- | Stampa un intero DATA / NDATA ([Id]) riga per riga con numerazione
prettyPrintData :: String -> DATA -> IO ()
prettyPrintData label dataList = do
  putStrLn $ "\n" ++ replicate 10 '=' ++ " " ++ label ++ " (" ++ show (length dataList) ++ " elementi) " ++ replicate 10 '='
  mapM_ (\(i, item) -> putStrLn $ show i ++ ". " ++ prettyId item) (zip [1..] dataList)
  putStrLn $ replicate (22 + length label + length (show (length dataList))) '='

prettyPrintTokens :: String -> [Token] -> IO ()
prettyPrintTokens title [] = putStrLn $ "=== " ++ title ++ " (Vuoto) ==="
prettyPrintTokens title toks = do
    putStrLn $ "\n=== " ++ title ++ " (" ++ show (length toks) ++ " token) ==="
    mapM_ printToken toks
  where
    printToken :: Token -> IO ()
    printToken (id', lab, addr) = do
        -- Scompattiamo l'Id interno
        let (term, pol, pos, seq, pi) = id'
            posSeqStr = case seq of
                Concl       -> "CONCL"
                Prem name n -> "PREM(" ++ name ++ "," ++ show n ++ ")"
        
        -- Stampa formattata sulla stessa linea
        putStrLn $ "TOKEN " 
                ++ "| Lab: " ++ show lab 
                ++ " | Addr: " ++ show addr 
                ++ " | Pol: " ++ show pol 
                ++ " | Seq: " ++ posSeqStr 
                ++ " | LR: " ++ show pos 
                ++ " | Pi: " ++ show pi 
                ++ " | Term: " ++ show term
