module CircuitGraph where

--import Data.Map (Map)
import Data.Map as Map
import qualified Data.Set as Set
import TypeTree (Type(..),Term(..),TypedTerm(..),Value(..),TypedValue(..))
import CreateDerivation (Prem(..),Concl(..),TypeDerivation(..),Tree(..))
import Debug.Trace (trace)

type Address = Map.Map String String
emptyAddress :: Address
emptyAddress = Map.empty
data Position = P | N | L | R deriving (Eq, Ord, Show)
type Id = [Position]
type Token = (Id, Address) 
type DATA = Set.Set Id
type NDATA = Set.Set Id

createTokens :: NDATA -> Set.Set Token
createTokens negativePos = Set.map (\idTok -> (idTok, Map.empty)) negativePos

extractInitialConf :: TypeDerivation -> (TypeDerivation, Set.Set Token, Set.Set Token)
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


extractData :: TypeDerivation -> Id -> (DATA, NDATA) 
extractData (Node concl forest) [] = 
     
    let (currentData, currentNData) = processConcl concl [P] -- La root e' sempre P 
        
    in
        (currentData, currentNData)


processConcl :: Concl -> Id -> (DATA, NDATA)
processConcl (_, _, typ) baseId = inspectType baseId typ
  where
    inspectType :: Id -> Type -> (DATA, NDATA)
    
    inspectType currentId TQbit = 
      case reverse currentId of
        [] -> (Set.singleton currentId, Set.empty)
        --Analizzo per mettere solo in DATA o anche in NDATA (PDATA differenza di insiemi)
        path -> case findLastPolarity path of
                  Just P  -> (Set.singleton currentId, Set.empty)
                  Just N  -> (Set.singleton currentId, Set.singleton currentId)

    -- Positivo nella concl negativo nell;antecedente
    inspectType currentId (TFun t1 t2) =
      let (d1, nd1) = inspectType (currentId ++ [N]) t1
          (d2, nd2) = inspectType (currentId ++ [P]) t2
      in (Set.union d1 d2, Set.union nd1 nd2)

    -- Mantiene la stessa posizione su entrambi i lati L R per identificare
    inspectType currentId (TPair t1 t2) =
      let (d1, nd1) = inspectType (currentId ++ [L]) t1
          (d2, nd2) = inspectType (currentId ++ [R]) t2
      in (Set.union d1 d2, Set.union nd1 nd2)


findLastPolarity :: [Position] -> Maybe Position
findLastPolarity [] = Nothing
findLastPolarity (x:xs)
  | x == P || x == N = Just x
  | otherwise       = findLastPolarity xs

