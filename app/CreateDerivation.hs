module CreateDerivation where
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
startDerivation t = buildDerivation Map.empty t



buildDerivation :: Prem -> TypedTerm -> TypeDerivation
buildDerivation prem term = case term of
  -- Caso TV: Derivazione valore
  TV innerTerm t -> 
    let innerTree = buildDerivationV prem innerTerm t
    in Node 
         { rootLabel = (prem, TV innerTerm t, t)
         , subForest = [innerTree] 
         }

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
         , subForest = gateLeaf : childTrees 
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



---Utils

getGateType :: String -> Type
getGateType "CNOT" = 
  let qpair = TPair TQbit TQbit 
  in TFun qpair qpair                     -- (Q x Q) -> (Q x Q)
getGateType _      = TFun TQbit TQbit

typeOf :: TypedTerm -> Type
typeOf (TV _ t)            = t
typeOf (TApp _ _ t)        = t
typeOf (TGate _ _ t)       = t
typeOf (TLet _ _ _ _ t)    = t
typeOf (TDecomp _ _ _ _ t) = t
typeOf (TIf _ _ _ t)       = t

-- Funzione principale di stampa
prettyPrintDerivation :: TypeDerivation -> String
prettyPrintDerivation tree = go 0 tree
  where
    go :: Int -> TypeDerivation -> String
    go indent (Node (prem, term, typ) subs) =
      let 
        indentStr = replicate (indent * 2) ' '
        
        -- Formattazione delle premesse: {x : Qbit, y : Qbit}
        premList  = [k ++ " : " ++ show v | (k, v) <- Map.toList prem]
        premStr   = "{" ++ intercalate ", " premList ++ "}"
        
        -- Il giudizio di tipo: {Gamma} |- Termine : Tipo
        judgement = premStr ++ " |- " ++ showTermPretty term ++ " : " ++ show typ
        
        -- Stampa delle sotto-premesse (figli dell'albero)
        childrenStr = case subs of
          [] -> ""
          _  -> "\n" ++ intercalate "\n" (map (go (indent + 1)) subs)
      in
        indentStr ++ "|-- " ++ judgement ++ childrenStr

-- Stampa direttamente a schermo
printDerivation :: TypeDerivation -> IO ()
printDerivation deriv = putStrLn (prettyPrintDerivation deriv)



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
