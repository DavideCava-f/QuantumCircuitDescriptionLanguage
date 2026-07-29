module CreateDerivation where

import qualified Data.Set as Set
import qualified Data.Map as Map
import TypeTree (Type(..),Term(..),TypedTerm(..),Value(..),TypedValue(..))
import Data.List (intercalate)

type Prem = Map.Map String Type
type Concl = (Prem, TypedTerm, Type)

data Tree a = Node -- Generic a
  { rootLabel :: a
  , subForest :: [Tree a] 
  } deriving (Show, Eq)

type TypeDerivation = Tree Concl

startDerivation :: TypedTerm -> TypeDerivation
startDerivation t = 
    let 
        tree = buildDerivation Map.empty t
    in
        cleanDerivationTree tree



buildDerivation :: Prem -> TypedTerm -> TypeDerivation
buildDerivation prem term = case term of
  -- Caso TV: Derivazione valore
  TV innerTerm t ->  buildDerivationV prem innerTerm t

  -- Caso TGate: Risolve le premesse ricorsivamente per ogni argomento, oltre che aggiungere il tipo del gate
  TGate g args t -> 
    let 

     childTrees = map (buildDerivation prem) args

     gateType = getGateType g
      
     gateLeaf = Node 
                 { rootLabel = (prem, TV (TVar g gateType) gateType, gateType)
                 , subForest = []
                 }
    in Node 
         { rootLabel = (prem, TGate g args t, t)
         , subForest =  gateLeaf : childTrees  
         }
  TLet x xType val body t -> 
    let 
        -- 1. Derivazione del valore assegnato al let (nel contesto corrente)
     valTree = buildDerivation prem val
        
        -- 2. Estensione del contesto con la nuova variabile x
     extPrem = Map.insert x xType prem
        
        -- 3. Derivazione del corpo del let (nel contesto esteso)
     bodyTree = buildDerivation extPrem body
        
    in Node 
        { rootLabel = (prem, TLet x xType val body t, t)
        , subForest = [valTree, bodyTree] -- I due rami delle premesse
        }
  TDecomp x y pair body t -> 
      let 
        -- Derivazione premessa da destrutturare 
        pairTree = buildDerivation prem pair
        
       -- Estrazione tipi delle singole variabili 
        (xType, yType) = case typeOf pair of
                           TPair t1 t2 -> (t1, t2)
        
      -- Aggiornamento contesto 
        extPrem = Map.insert x xType (Map.insert y yType prem)
        
        -- Derivazione del corpo
        bodyTree = buildDerivation extPrem body
        
      in Node 
           { rootLabel = (prem, TDecomp x y pair body t, t)
           , subForest = [pairTree, bodyTree] 
           }

  TApp f x t -> 
    let 
        -- 1. Derivazione del valore applicante
     appl = buildDerivation prem f
        
        
        -- 3. Derivazione del valore applicato
     applied = buildDerivation prem x
        
    in Node 
        { rootLabel = (prem, TApp f x t,t)
        , subForest = [appl, applied] -- I due rami
        }

  TIf cond branch1 branch2 t -> 
    let 
        -- 1. Derivazione condiizione
     condDer = buildDerivation prem cond
        
        

        -- 1. Derivazione then
     branch1Der = buildDerivation prem branch1
        -- 1. Derivazione esle
     branch2Der = buildDerivation prem branch2
        
    in Node 
        { rootLabel = (prem, TIf cond branch1 branch2 t,t)
        , subForest = [condDer, branch1Der, branch2Der] -- I dtre rami
        }
buildDerivationV :: Prem -> TypedValue -> Type -> TypeDerivation
buildDerivationV prem val t = case val of
-- Caso TVar: Foglia (nessuna premessa aggiunta)
  TVar y varType -> 
    Node 
      { rootLabel = (prem, TV (TVar y varType) t, t)
      , subForest = [] 
      }


  -- Caso TLambda: Estende il contesto (Prem) con la nuova variabile 'y'
  TLambda y argT body lamType -> 
    let extPrem  = Map.insert y argT prem
        bodyTree = buildDerivation extPrem body
    in Node 
         { rootLabel = (prem, TV (TLambda y argT body lamType) t, t)
         , subForest = [bodyTree] 
         }

  TTensor t1 t2 tensorType ->   
    let t1Tree = buildDerivation prem t1
        t2Tree = buildDerivation prem t2
    in Node 
         { rootLabel = (prem, TV (TTensor t1 t2 tensorType) t, t)
         , subForest = [t1Tree, t2Tree] 
         }


--- Cleaning Tree

freeVarsTerm :: TypedTerm -> Set.Set String
freeVarsTerm term = case term of
  TV val _                 -> freeVarsVal val
  TGate _ args _           -> Set.unions (map freeVarsTerm args)
  TApp f arg _             -> Set.union (freeVarsTerm f) (freeVarsTerm arg)
  TLet x _ val body _      -> Set.union (freeVarsTerm val) (Set.delete x (freeVarsTerm body))
  TDecomp x y pair body _  -> Set.union (freeVarsTerm pair) (freeVarsTerm body Set.\\ Set.fromList [x, y])
  TIf c t e _              -> Set.unions [freeVarsTerm c, freeVarsTerm t, freeVarsTerm e]

freeVarsVal :: TypedValue -> Set.Set String
freeVarsVal val = case val of
  TVar x _                 -> Set.singleton x
  TLambda x _ body _       -> Set.delete x (freeVarsTerm body)
  TTensor t1 t2 _          -> Set.union (freeVarsTerm t1) (freeVarsTerm t2)

cleanDerivationTree :: TypeDerivation -> TypeDerivation
cleanDerivationTree (Node (prem, term, typ) subs) =
    let 
      -- Trova le variabili utilizzate nel termine
      usedVars     = freeVarsTerm term
      
      -- Prendi solo le variabili utili
      filteredPrem = Map.restrictKeys prem usedVars
      
      -- chiamata ricorsiva
      cleanedSubs  = map cleanDerivationTree subs
    in 
      -- Restituisce un nuovo nodo con la 'filteredPrem' al posto di 'prem'
      Node (filteredPrem, term, typ) cleanedSubs

-- Utils

getGateType :: String -> Type
getGateType "CNOT" = 
  let qpair = TPair TQbit TQbit 
  in TFun qpair qpair                     -- (Q x Q) -> (Q x Q)
getGateType "M"      = TFun TQbit TBit
getGateType _      = TFun TQbit TQbit

typeOf :: TypedTerm -> Type
typeOf (TV _ t)            = t
typeOf (TApp _ _ t)        = t
typeOf (TGate _ _ t)       = t
typeOf (TLet _ _ _ _ t)    = t
typeOf (TDecomp _ _ _ _ t) = t
typeOf (TIf _ _ _ t)       = t

-- Pretty Print
prettyPrintDerivation :: TypeDerivation -> String
prettyPrintDerivation tree = go 0 tree
  where
    go :: Int -> TypeDerivation -> String
    go indent (Node (prem, term, typ) subs) =
      let 
        indentStr = replicate (indent * 2) ' '
        
        --usedVars = freeVarsTerm term       
        --filteredPrem = Map.restrictKeys prem usedVars
        -- Formattazione delle premesse: {x : Qbit, y : Qbit}
        premList  = [k ++ " : " ++ showTypePretty v | (k, v) <- Map.toList prem]
        premStr   = "{" ++ intercalate ", " premList ++ "}"
        
        -- Il giudizio di tipo: {Gamma} |- Termine : Tipo
        judgement = premStr ++ " |- " ++ showTermPretty term ++ " : " ++ showTypePretty typ
        
        -- Stampa delle sotto-premesse (figli dell'albero)
        childrenStr = case subs of
          [] -> ""
          _  -> "\n" ++ intercalate "\n" (map (go (indent + 1)) subs)
      in
        indentStr ++ "|-- " ++ judgement ++ childrenStr


-- Stampa direttamente a schermo
printDerivation :: TypeDerivation -> IO ()
printDerivation deriv = putStrLn (prettyPrintDerivation deriv)

colorQbit :: String -> String
colorQbit s = "\ESC[1;36m" ++ s ++ "\ESC[0m"   -- Ciano Brillante

colorTFun :: String -> String
colorTFun s = "\ESC[1;35m" ++ s ++ "\ESC[0m"   -- Magenta Brillante

colorTPair :: String -> String
colorTPair s = "\ESC[1;33m" ++ s ++ "\ESC[0m"


showTypePretty :: Type -> String
showTypePretty TQbit        = colorQbit "qbit"
showTypePretty TBit        = colorQbit "bit"
showTypePretty (TFun t1 t2) = colorTFun "TFUN" ++ " (" ++ showTypePretty t1 ++ " -> " ++ showTypePretty t2 ++ ")"
showTypePretty (TPair t1 t2)= colorTPair "TPAIR" ++ " (" ++ showTypePretty t1 ++ ", " ++ showTypePretty t2 ++ ")"
showTypePretty t            = show t -- Fallback per altri tipi non specificati

showTermPretty :: TypedTerm -> String
showTermPretty term = case term of
  TV val _                   -> showValPretty val
  TGate g args _             -> g ++ "(" ++ intercalate ", " (map showTermPretty args) ++ ")"
  TApp f arg _               -> "(" ++ showTermPretty f ++ " " ++ showTermPretty arg ++ ")"
  TLet x _ val body _        -> "let " ++ x ++ " = " ++ showTermPretty val ++ " in " ++ showTermPretty body
  TDecomp x y pair body _    -> "let (" ++ x ++ ", " ++ y ++ ") = " ++ showTermPretty pair ++ " in " ++ showTermPretty body
  TIf c t e _                -> "if " ++ showTermPretty c ++ " then " ++ showTermPretty t ++ " else " ++ showTermPretty e

showValPretty :: TypedValue -> String
showValPretty val = case val of
  TVar x _                   -> x
  TLambda x argT body _      -> "(\\" ++ x ++ ":" ++ show argT ++ ". " ++ showTermPretty body ++ ")"
  TTensor t1 t2 _            -> "(" ++ showTermPretty t1 ++ " (x) " ++ showTermPretty t2 ++ ")"
