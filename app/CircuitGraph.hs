module CircuitGraph where

--import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeTree (Type(..),Term(..),TypedTerm(..),Value(..),TypedValue(..))
import CreateDerivation (Prem(..),Concl(..),TypeDerivation(..),Tree(..))
import Debug.Trace (trace)

type Address = Map.Map String String
data PosInSeq = Concl | Prem Int deriving (Show)
type PosInPi = Int 
emptyAddress :: Address
emptyAddress = Map.empty
data Position = L | R deriving (Show)
data Polarity = P | N deriving (Show)
type Id = (TypedTerm, Polarity, [Position], PosInSeq, [PosInPi])-- deriving (Show)
type Token = (Id, Address) 
type DATA = [Id]
type NDATA = [Id]



{--extractInitialConf :: TypeDerivation -> (TypeDerivation, Set.Set Token, Set.Set Token)
extractInitialConf pi =
    let 
        -- 1. Estraiamo i Set complessivi (DATA e NDATA)
        -- Nota: passiamo l'Id iniziale vuoto [] a extractData
        (totalData, negativePos) = extractData pi []
        
        -- 2. Creiamo i token associando l'indirizzo vuoto Map.empty
        tokens = createTokens negativePos
        
        -- 3. Inizializziamo l'Extended Circuit (per ora vuoto)
        extendedCircuit = Set.empty
    in
        (pi, tokens, extendedCircuit)   
--}

extractAllData :: TypeDerivation -> (DATA, NDATA)
extractAllData derivation = extractDataRecursive derivation []


extractDataRecursive :: TypeDerivation -> [PosInPi] -> (DATA, NDATA)
extractDataRecursive (Node concl forest) pathPi =
    let 
        -- 1. Estraiamo DATA e NDATA dal nodo corrente (usando il suo pathPi)
        (currentData, currentNData) = processJudgment concl pathPi
        
        processForest :: [TypeDerivation] -> Int -> (DATA, NDATA)
        processForest [] _ = ([], [])
        processForest (child : cs) idx =
            let (childD, childND) = extractDataRecursive child (pathPi ++ [idx])
                (restD,  restND)  = processForest cs (idx + 1)
            in (childD ++ restD, childND ++ restND)

        (subData, subNData) = processForest forest 0
            
    in
        -- 4. Uniamo tutto in un unico grande insieme DATA e NDATA
        (currentData ++ subData, currentNData ++ subNData)

processJudgment :: Concl -> [PosInPi] -> (DATA, NDATA)
processJudgment (prems, conclTerm, typ) pathPi = 
  let
    -- Launch 1: Processa tutte le premesse (Prem)
    (premData, premNData) = processPremises prems conclTerm pathPi
    
    -- Launch 2: Processa la conclusione (Type)
    (conclData, conclNData) = processConcl conclTerm typ pathPi
  in
    -- Unisce i risultati di entrambi
    ( premData ++ conclData
    , premNData ++ conclNData
    )

processPremises :: Prem -> TypedTerm -> [PosInPi] -> (DATA, NDATA)
processPremises premMap typedTerm pathPi =
    processList (Map.toList premMap) 0
  where
    -- Funzione ausiliaria che scorre la lista di coppie (NomeVariabile, Type)
    processList :: [(String, Type)] -> Int -> (DATA, NDATA)
    processList [] _ = ([], [])
    processList ((_, typ) : rest) premIdx =
      let 
        -- 1. Estrae DATA e NDATA per la premessa corrente
        (currentD, currentND) = inspectPremiseType [] True typ premIdx
        
        -- 2. Ricorsione sulle premesse rimanenti con indice incrementato
        (restD, restND)       = processList rest (premIdx + 1)
      in 
        -- 3. Unisce i risultati direttamente in una singola coppia
        (currentD ++ restD, currentND ++ restND)

    inspectPremiseType :: [Position] -> Bool -> Type -> Int -> (DATA, NDATA)
    inspectPremiseType lrPath isPositive TQbit premIdx =
      if isPositive
        then 
            let currentId = (typedTerm, N, lrPath, Prem premIdx, pathPi) in ([currentId], [])
        else
            let currentId = (typedTerm, P, lrPath, Prem premIdx, pathPi) in ([currentId], [currentId])



    inspectPremiseType lrPath isPositive (TFun t1 t2) premIdx =
      let (d1, nd1) = inspectPremiseType (lrPath ++ [L]) (not isPositive) t1 premIdx
          (d2, nd2) = inspectPremiseType (lrPath ++ [R]) isPositive t2 premIdx
      in (d1 ++ d2, nd1 ++ nd2)

    inspectPremiseType lrPath isPositive (TPair t1 t2) premIdx =
      let (d1, nd1) = inspectPremiseType (lrPath ++ [L]) isPositive t1 premIdx
          (d2, nd2) = inspectPremiseType (lrPath ++ [R]) isPositive t2 premIdx
      in (d1 ++ d2, nd1 ++ nd2)


processConcl :: TypedTerm -> Type -> [PosInPi] -> (DATA, NDATA)
processConcl typedTerm typ pathPi = inspectType [] True typ
  where
    inspectType :: [Position] -> Bool -> Type -> (DATA, NDATA)
    
    inspectType lrPath isPositive TQbit =
      if isPositive
        then 
            let currentId = (typedTerm, P, lrPath, Concl, pathPi) in ([currentId], [])
        else
            let currentId = (typedTerm, N, lrPath, Concl, pathPi) in ([currentId], [currentId])

    inspectType lrPath isPositive (TFun t1 t2) =
      let (d1, nd1) = inspectType (lrPath ++ [L]) (not isPositive) t1
          (d2, nd2) = inspectType (lrPath ++ [R]) isPositive t2
      in (d1 ++ d2, nd1 ++ nd2)

    inspectType lrPath isPositive (TPair t1 t2) =
      let (d1, nd1) = inspectType (lrPath ++ [L]) isPositive t1
          (d2, nd2) = inspectType (lrPath ++ [R]) isPositive t2
      in (d1 ++ d2, nd1 ++ nd2)

