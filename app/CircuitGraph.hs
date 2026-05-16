module CircuitGraph where


--import Data.Map (Map)
import Data.Map as Map
import TypeTree (Type(..),Term(..),TypedTerm(..),Value(..),TypedValue(..))
import Debug.Trace (trace)
--Rappresentazione del grafo come lista di nodi e archi

type Name = String
type NodeID = Int --Il nodo, puo essere gate con piu' entrate
type PortID = Int -- Distingue le entrate in un nodo

data Outcome = Single (NodeID, PortID)  | Pair Outcome Outcome -- Nodi di Outcome della visita
    deriving (Show) 
--type WireLocation = (NodeID, PortID)
--type WireMap = Map.Map Name Outcome
data NodeContent
  = InPort Name        -- Vari ingressi del programma
  | QGate String       -- H, X, Z...
  deriving (Show)

data Node = Node
  { nodeID  :: NodeID
  , content :: NodeContent
  } deriving (Show)

data Edge = Edge (NodeID, PortID) (NodeID, PortID) deriving (Show)

data Graph = Graph 
    { nodes :: Map NodeID NodeContent
    , edges :: [Edge]
    , nextID :: NodeID -- Per generare ID univoci
    } deriving (Show)

startGraphCreat :: TypedTerm -> (Graph, Outcome) -- Creo grafo vuoto e lancio la visita
startGraphCreat ast =
    let
        -- Partiamo con un grafo realmente vuoto
        emptyGraph = Graph 
            { nodes  = Map.empty
            , edges  = []
            , nextID = 0 
            }
            
        emptyWmap = Map.empty
        
        (_, finalGraph, lastPort) = visit ast emptyWmap emptyGraph
    in
        (finalGraph, lastPort)

type WireMap = Map.Map Name Outcome


visit :: TypedTerm -> WireMap -> Graph -> (WireMap, Graph, Outcome)
visit tt wmap g = trace ("--- Entro in visit con: " ++ show tt) $ case tt of
    -- Cerca la variabile nella wmap
    TV val ty -> visitValue val ty wmap g

    TGate name args ty ->
        let (wmap1, g1, outcomes) = visitArgs args wmap g
            sources = concatMap flattenOutcome outcomes
            gateID = trace ("--- Sources: " ++ show sources) nextID g1
            newNodes = Map.insert gateID (QGate name) (nodes g1)
            newEdges = [ Edge src (gateID, port) | (src, port) <- zip sources [0..] ]
            g2 = g1 { nodes = newNodes
                    , edges = edges g1 ++ newEdges
                    , nextID = gateID + 1 }
            finalOutcome = case name of
                "CNOT" -> Pair (Single (gateID, 0)) (Single (gateID, 1))
                _      -> Single (gateID, 0)
        in (wmap1, g2, finalOutcome)

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

visitValue :: TypedValue -> Type -> WireMap -> Graph -> (WireMap, Graph, Outcome)
visitValue tv ty wmap g = case tv of
    TVar x _ -> 
        case Map.lookup x wmap of
            Just out -> (wmap, g, out)
            Nothing  -> error $ "Variable " ++ x ++ " not found"

-- Generazione di porte di input in base al tipo di lambda
    TLambda x tyArg body tyTot -> 
        let genInPorts TQbit (graph, currID) = 
                let g' = graph { nodes = Map.insert currID (InPort x) (nodes graph)
                               , nextID = currID + 1 }
                in (g', Single (currID, 0))

            genInPorts (TPair t1 t2) (graph, currID) =
                let (g1, out1) = genInPorts t1 (graph, currID)
                    (g2, out2) = genInPorts t2 (g1, nextID g1)
                in (g2, Pair out1 out2)

            genInPorts (TFun _ _) (graph, currID) = 
                (graph, error "ToUpdate")

            (gFinal, initialOutcome) = genInPorts tyArg (g, nextID g)
            wmapWithX = Map.insert x initialOutcome wmap
        in visit body wmapWithX gFinal

    TTensor t1 t2 _ ->
        let (wmap1, g1, out1) = visit t1 wmap g
            (wmap2, g2, out2) = visit t2 wmap1 g1
        in (wmap2, g2, Pair out1 out2)
 
 --------Utils
visitArgs :: [TypedTerm] -> WireMap -> Graph -> (WireMap, Graph, [Outcome]) --Visita la lista di argomenti
visitArgs [] wmap g = (wmap, g, [])
visitArgs (t:ts) wmap g = 
    let (wmap1, g1, out) = visit t wmap g
        (wmap2, g2, outs) = visitArgs ts wmap1 g1
    in (wmap2, g2, out : outs)

flattenOutcome :: Outcome -> [(NodeID, PortID)] --Utility per le sources
flattenOutcome (Single p) = [p]
flattenOutcome (Pair o1 o2) = flattenOutcome o1 ++ flattenOutcome o2

