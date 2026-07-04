module CircuitGraph where


--import Data.Map (Map)
import Data.Map as Map
import TypeTree (Type(..),Term(..),TypedTerm(..),Value(..),TypedValue(..))
import Debug.Trace (trace)
--Rappresentazione del grafo come lista di nodi e archi

type Name = String
type NodeID = Int --Il nodo, puo essere gate con piu' entrate
type InTermID = Int --Il nodo, puo essere gate con piu' entrate
-- type PortID = Int -- Distingue le entrate in un nodo

--data Outcome = Single (NodeID, PortID)  | Pair Outcome Outcome -- Nodi di Outcome della visita
  --  deriving (Show) 
--type WireLocation = (NodeID, PortID)
--type WireMap = Map.Map Name Outcome
data NodeContent
  = NodeName Name NodeID
  deriving (Show)

data Node = Node
  { nodeID  :: NodeID
  , content :: NodeContent
  } deriving (Show)

data Edge = Edge (NodeID,NodeID) deriving (Show)

data Graph = Graph 
    { nodes :: Map NodeID Node
    , edges :: [Edge]
    , nextID :: NodeID -- Per generare ID univoci
    , nextTermID :: NodeID -- Per generare ID univoci
    } deriving (Show)

startGraphCreat :: TypedTerm -> Graph -- Creo grafo vuoto e lancio la visita
startGraphCreat ast =
    let
        emptyGraph = Graph 
            { nodes  = Map.empty
            , edges  = []
            , nextID = 0 
            , nextTermID = 0 
            }
            
        emptyWmap = Map.empty
        
        finalGraph = visit ast emptyGraph
    in
        finalGraph

--type WireMap = Map.Map Name Outcome


visit :: TypedTerm -> Graph -> Graph
visit tt g = trace ("--- Entro in visit con: " ++ show tt) $ case tt of
    -- Cerca la variabile nella wmap
    TV val ty -> visitValue val ty g


-- Chiamare Visit su body, 
visitValue :: TypedValue -> Type -> Graph -> Graph-- (WireMap, Graph, Outcome)
visitValue tv ty g = case tv of
--    TVar x _ -> 
-- Generazione di porte di input in base al tipo di lambda
-- Incrementale automatico del idTerm, Funzoine di Connect
    TLambda x tyArg body tyTot -> 
        case tyTot of
            TFun t1 t2 -> 
                let g1 = createNodes tyArg x 0 g 
                    g2 = createNodes t1 "f1i" 1 g1        
                    g3 = createNodes t2 "f1o" 2 g2 
                    in connect "f1i" x g3

 --------Utils

--Contare il numero Qbits attivi
countQbits :: Type -> Int 
countQbits tipo = case tipo of
  
  TQbit -> 1  
  TFun t1 t2 ->
    let tot  = countQbits t1 + countQbits t2 
    in tot

  TPair t1 t2 ->
    let tot  = countQbits t1 + countQbits t2 
    in tot
{--
createNodes :: Type -> Name -> NodeID -> Graph -> Graph --Visita la lista di argomenti
createNodes tipo nome id g = case tipo of
  
  -- CASO BASE: Abbiamo trovato un Qubit! Creiamo il nodo nel grafo.
  TQbit ->
    let uid = nextID g  -- Prendiamo l'ID corrente
        termUid = nextTermID g  -- Prendiamo l'ID corrente
        
        -- 1. Aggiorniamo l'anagrafe dei nodi
        nuovoNodo = Node { nodeID = uid, content = NodeName nome termUid }
        
        -- 2. Aggiorniamo la mappa di lookup per il nome (inserendo l'ID nella lista)
        -- nuovoNameToIds = M.insertWith (++) nome [uid] (nameToIds g)
        nuoviNodi = Map.insert uid nuovoNodo (nodes g)
        
        in g { nextID = uid + 1               -- Incrementiamo il contatore per il prossimo nodo
            , nextTermID = termUid + 1
            , nodes = nuoviNodi
            }

  -- CASO RICORSIVO: Il Pair contiene più qubit. Scompattiamo prima a sinistra, poi a destra.
  TPair t1 t2 ->
    let g'  = createNodes t1 nome id g   -- Espande t1 usando il grafo di partenza
        g'' = createNodes t2 nome id g'  -- Espande t2 usando il grafo aggiornato da t1
    in g''

  -- CASO RICORSIVO: Anche la Funzione trasporta qubit. Stessa logica a catena.
  TFun t1 t2 ->
    let g'  = createNodes t1 nome id g
        g'' = createNodes t2 nome id g'
    in g''
--}    
createNodes :: Type -> Name -> NodeID -> Graph -> Graph
createNodes tipo nome idTerm g =
    -- Chiamiamo la funzione ricorsiva vera e propria
    let grafoElaborato = go tipo nome idTerm g
    -- Alla fine dell'intera esecuzione, azzeriamo il contatore nextTermID nel grafo finale
    in grafoElaborato { nextTermID = 0 }

  where
    -- 2. LA FUNZIONE RICORSIVA LOCALE (go)
    -- Lavora esattamente come prima, passando il grafo come testimone
    go :: Type -> Name -> NodeID -> Graph -> Graph
    go t n i graph = case t of
      
      TQbit ->
        let uid       = nextID graph
            termUid   = nextTermID graph
            nuovoNodo = Node { nodeID = uid, content = NodeName n termUid }
            nuoviNodi = Map.insert uid nuovoNodo (nodes graph)
        in graph { nextID     = uid + 1
                 , nextTermID = termUid + 1
                 , nodes      = nuoviNodi
                 }

      TPair t1 t2 ->
        let g'  = go t1 n i graph
            g'' = go t2 n i g'
        in g''

      TFun t1 t2 ->
        let g'  = go t1 n i graph
            g'' = go t2 n i g'
        in g''


connect :: Name -> Name -> Graph -> Graph
connect nomeA nomeB g =
    let -- 1. Estraiamo tutti i nodi dal grafo come lista
        tuttiINodi = Map.elems (nodes g)
        
        -- 2. Filtriamo i nodi che appartengono al primo nome (nomeA)
        nodiA = [ (uid, tid) | Node uid (NodeName n tid) <- tuttiINodi, n == nomeA ]
        
        -- 3. Filtriamo i nodi che appartengono al secondo nome (nomeB)
        nodiB = [ (uid, tid) | Node uid (NodeName n tid) <- tuttiINodi, n == nomeB ]
        
        -- 4. Creiamo gli archi accoppiando i nodi che hanno lo STESSO tid (ID del termine)
        nuoviArchi = [ Edge (uidA, uidB) | (uidA, tidA) <- nodiA
                                    , (uidB, tidB) <- nodiB
                                    , tidA == tidB ]
                                    
    in g { edges = nuoviArchi ++ edges g }

{-
visitArgs :: [TypedTerm] -> WireMap -> Graph -> (WireMap, Graph, [Outcome]) --Visita la lista di argomenti
visitArgs [] wmap g = (wmap, g, [])
visitArgs (t:ts) wmap g = 
    let (wmap1, g1, out) = visit t wmap g
        (wmap2, g2, outs) = visitArgs ts wmap1 g1
    in (wmap2, g2, out : outs)

flattenOutcome :: Outcome -> [(NodeID, PortID)] --Utility per le sources
flattenOutcome (Single p) = [p]
flattenOutcome (Pair o1 o2) = flattenOutcome o1 ++ flattenOutcome o2
-}


{- To create
 -
    TDecomp y z t1 t2 _ ->
        let (wmap1, g1, outcome) = visit t1 wmap g 
        in case outcome of
            Pair outY outZ ->
                let wmapWithYZ = Map.insert y outY (Map.insert z outZ wmap1)
                in visit t2 wmapWithYZ g1
            _ -> error "Expected a pair for decomposition"

    TLet x tyX t1 t2 tyTot ->
        let (wmap1, g1, out1) = visit t1 wmap g
            wmapWithX = Map.insert x out1 wmap1
            (wmap2, g2, outFinal) = visit t2 wmapWithX g1
            finalWmap = Map.delete x wmap2
        in (finalWmap, g2, outFinal)

    TApp f arg _ ->
        let (wmap1, g1, outArg) = visit arg wmap g
            in case f of

            -- Caso applico lambda
                TV (TLambda x _ body _) _ ->
                    let wmapWithArg = Map.insert x outArg wmap1
                    in visit body wmapWithArg g1
                    
            -- Caso applico Var
                TV (TVar name _) _ ->
                    -- Creo nodi e archi
                    let gateID = nextID g1
                        newNodes = Map.insert gateID (QGate name) (nodes g1)
                        
                        sources = flattenOutcome outArg
                        newEdges = [ Edge src (gateID, port) | (src, port) <- zip sources [0..] ]
                        
                        g2 = g1 { nodes = newNodes
                                , edges = edges g1 ++ newEdges
                                , nextID = gateID + 1 }
                        
                        -- Definizione uscita
                        finalOutcome = case name of
                            "CNOT" -> Pair (Single (gateID, 0)) (Single (gateID, 1))
                            _      -> Single (gateID, 0)
                    in (wmap1, g2, finalOutcome)
    TTensor t1 t2 _ ->
        let (wmap1, g1, out1) = visit t1 wmap g
            (wmap2, g2, out2) = visit t2 wmap1 g1
        in (wmap2, g2, Pair out1 out2)
 
 -}


