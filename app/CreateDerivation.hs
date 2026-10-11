module CreateDerivation where

import qualified Data.Set as Set
import qualified Data.Map as Map
import TypeTree (Type(..),TypedTerm(..),TypedValue(..),getTType)
import Data.List (intercalate)

type Prem = Map.Map String Type
type Concl = (Prem, TypedTerm, Type)

data Tree a = Node -- Generic a
  { rootLabel :: a
  , subForest :: [Tree a] 
  } deriving (Show, Eq)

type TypeDerivation = Tree Concl

startDerivation :: Prem -> TypedTerm -> TypeDerivation
startDerivation prem t = 
    let 
        tree = buildDerivation prem t
    in
        cleanDerivationTree tree

buildDerivation :: Prem -> TypedTerm -> TypeDerivation
buildDerivation prem term = case term of
  -- TV case: derivation of a value
  TV innerTerm t ->  buildDerivationV prem innerTerm t

  TNew n varId t ->
    Node
      { rootLabel = (Map.empty, TNew n varId t, t)
      , subForest = []
      }

  -- TGate Case: Recursively resolves the premises for each argument, in addition to adding the gate type.
  TGate g args t -> 
    let 

     childTrees = map (buildDerivation prem) args

     gateType = getGateType g args t
      
     gateLeaf = Node 
                 { rootLabel = (prem, TGate g [] gateType, gateType)
                 , subForest = []
                 }
    in Node 
         { rootLabel = (prem, TGate g args t, t)
         , subForest =  gateLeaf : childTrees  
         }
  TLet x xType val body t -> 
    let 
        -- 1. Derivation of the value assigned to the let (in the current context)
     valTree = buildDerivation prem val
        
        -- 2. Extension of the context with the new variable x
     extPrem = Map.insert x xType prem
        
        -- 3. Derivation of the let-body (in the extended context)
     bodyTree = buildDerivation extPrem body
        
    in Node 
        { rootLabel = (prem, TLet x xType val body t, t)
        , subForest = [valTree, bodyTree] -- The two branches of the premises
        }
  TDecomp names pair body t -> 
      let 
        -- Premise derivation to be deconstructed 
        pairTree = buildDerivation prem pair
        
       -- Extraction of the component types
        compTypes = case getTType pair of
                      TPair ts -> ts
                      other    -> error ("Decomposition of a non-tuple type: " ++ show other)
        
      -- Context update: one variable per component
        extPrem = foldr (uncurry Map.insert) prem (zip names compTypes)
        
        -- Body derivation
        bodyTree = buildDerivation extPrem body
        
      in Node 
           { rootLabel = (prem, TDecomp names pair body t, t)
           , subForest = [pairTree, bodyTree] 
           }

  TApp f x t -> 
    let 
        -- 1. Derivation of the applying value
     appl = buildDerivation prem f
        
        
        -- 3. Derivation of the applied value
     applied = buildDerivation prem x
        
    in Node 
        { rootLabel = (prem, TApp f x t,t)
        , subForest = [appl, applied] -- The two branches
        }

  TIf cond branch1 branch2 t -> 
    let 
        -- 1. Derivation of the condition
     condDer = buildDerivation prem cond
        
        

        -- 1. Derivation of the then branch
     branch1Der = buildDerivation prem branch1
        -- 1. Derivation of the else branch
     branch2Der = buildDerivation prem branch2
        
    in Node 
        { rootLabel = (prem, TIf cond branch1 branch2 t,t)
        , subForest = [condDer, branch1Der, branch2Der] -- The three branches
        }
buildDerivationV :: Prem -> TypedValue -> Type -> TypeDerivation
buildDerivationV prem val t = case val of
-- TVar case: leaf (no premise added)
  TVar y varType -> 
    Node 
      { rootLabel = (prem, TV (TVar y varType) t, t)
      , subForest = [] 
      }



  -- TLambda case: extends the context (Prem) with the new variable 'y'
  TLambda y argT body lamType -> 
    let extPrem  = Map.insert y argT prem
        bodyTree = buildDerivation extPrem body
    in Node 
         { rootLabel = (prem, TV (TLambda y argT body lamType) t, t)
         , subForest = [bodyTree] 
         }

  TTensor ts tensorType ->   
    Node 
      { rootLabel = (prem, TV (TTensor ts tensorType) t, t)
      , subForest = map (buildDerivation prem) ts
      }


--- Cleaning Tree

freeVarsTerm :: TypedTerm -> Set.Set String
freeVarsTerm term = case term of
  TV val _                 -> freeVarsVal val
  TNew _ _ _               -> Set.empty
  TGate _ args _           -> Set.unions (map freeVarsTerm args)
  TApp f arg _             -> Set.union (freeVarsTerm f) (freeVarsTerm arg)
  TLet x _ val body _      -> Set.union (freeVarsTerm val) (Set.delete x (freeVarsTerm body))
  TDecomp xs pair body _   -> Set.union (freeVarsTerm pair) (freeVarsTerm body Set.\\ Set.fromList xs)
  TIf c t e _              -> Set.unions [freeVarsTerm c, freeVarsTerm t, freeVarsTerm e]

freeVarsVal :: TypedValue -> Set.Set String
freeVarsVal val = case val of
  TVar x _                 -> Set.singleton x
  TLambda x _ body _       -> Set.delete x (freeVarsTerm body)
  TTensor ts _             -> Set.unions (map freeVarsTerm ts)

cleanDerivationTree :: TypeDerivation -> TypeDerivation
cleanDerivationTree (Node (prem, term, typ) subs) =
    let 
      -- Find the variables used in the term
      usedVars     = freeVarsTerm term
      
      -- Keep only the variables that are used
      filteredPrem = Map.restrictKeys prem usedVars
      
      -- Recursive call
      cleanedSubs  = map cleanDerivationTree subs
    in 
      -- Returns a new node with 'filteredPrem' in place of 'prem'
      Node (filteredPrem, term, typ) cleanedSubs

-- Utils

getGateType :: String -> [TypedTerm] -> Type -> Type
getGateType _ [arg] t = TFun (getTType arg) t
getGateType g args  _ = error ("getGateType: gate " ++ g ++ " with " ++ show (length args) ++ " arguments")

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
        -- Formatting of the premises: {x : Qbit, y : Qbit}
        premList  = [k ++ " : " ++ showTypePretty v | (k, v) <- Map.toList prem]
        premStr   = "{" ++ intercalate ", " premList ++ "}"
        
        -- The typing judgement: {Gamma} |- Term : Type
        judgement = premStr ++ " |- " ++ showTermPretty term ++ " : " ++ showTypePretty typ
        
        -- Printing of the sub-premises (children of the tree)
        childrenStr = case subs of
          [] -> ""
          _  -> "\n" ++ intercalate "\n" (map (go (indent + 1)) subs)
      in
        indentStr ++ "|-- " ++ judgement ++ childrenStr


-- Print directly to the screen
printDerivation :: TypeDerivation -> IO ()
printDerivation deriv = putStrLn (prettyPrintDerivation deriv)

colorQbit :: String -> String
colorQbit s = "\ESC[1;36m" ++ s ++ "\ESC[0m"   -- Cian

colorTFun :: String -> String
colorTFun s = "\ESC[1;35m" ++ s ++ "\ESC[0m"   -- Magenta 

colorTPair :: String -> String
colorTPair s = "\ESC[1;33m" ++ s ++ "\ESC[0m"


showTypePretty :: Type -> String
showTypePretty TQbit        = colorQbit "qbit"
showTypePretty TBit        = colorQbit "bit"
showTypePretty (TFun t1 t2) = colorTFun "TFUN" ++ " (" ++ showTypePretty t1 ++ " -> " ++ showTypePretty t2 ++ ")"
showTypePretty (TPair ts)   = colorTPair "TPAIR" ++ " (" ++ intercalate ", " (map showTypePretty ts) ++ ")"
showTypePretty t            = show t -- Fallback for other unspecified types
showTermPretty :: TypedTerm -> String
showTermPretty term = case term of
  TV val _                   -> showValPretty val
  TGate g args _             -> g ++ "(" ++ intercalate ", " (map showTermPretty args) ++ ")"
  TNew _ varId _             -> varId
  TApp f arg _               -> "(" ++ showTermPretty f ++ " " ++ showTermPretty arg ++ ")"
  TLet x _ val body _        -> "let " ++ x ++ " = " ++ showTermPretty val ++ " in " ++ showTermPretty body
  TDecomp xs pair body _     -> "let <" ++ intercalate ", " xs ++ "> = " ++ showTermPretty pair ++ " in " ++ showTermPretty body
  TIf c t e _                -> "if " ++ showTermPretty c ++ " then " ++ showTermPretty t ++ " else " ++ showTermPretty e

showValPretty :: TypedValue -> String
showValPretty val = case val of
  TVar x _                   -> x
  TLambda x argT body _      -> "(\\" ++ x ++ ":" ++ show argT ++ ". " ++ showTermPretty body ++ ")"
  TTensor ts _               -> "<" ++ intercalate ", " (map showTermPretty ts) ++ ">"