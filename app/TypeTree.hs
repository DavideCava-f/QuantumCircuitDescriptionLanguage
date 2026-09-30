module TypeTree where

import Control.Monad (foldM)

type Name = String

data Type = TBit | TQbit | TFun Type Type | TPair Type Type
  deriving (Eq, Show)

data Term 
  = App Term Term                       -- H(x) CNOT(x,y) 
  | Let Name Type Term Term             -- let x = q1 in H(x)
  | Decomp Name Name Term Term          -- let <x,y> = t in t
  | If Term Term Term
  | New Int
  | Gate Name [Term]                  -- U(v1...vn) such as H, X, CNOT
  | V Value
  deriving (Show)
 
data Value
  = Var Name
  | Lambda Name Type Term
  | Tensor Term Term
  deriving (Show)

data TypedValue
  = TVar Name Type
  | TLambda Name Type TypedTerm Type    -- Lambda: arg, arg_type, typed_body, total_type
  | TTensor TypedTerm TypedTerm Type    -- Pair of two typed terms
  deriving (Show)

data TypedTerm
  = TV TypedValue Type
  | TApp TypedTerm TypedTerm Type
  | TNew Int String Type
  | TGate String [TypedTerm] Type
  | TLet Name Type TypedTerm TypedTerm Type
  | TDecomp Name Name TypedTerm TypedTerm Type
  | TIf TypedTerm TypedTerm TypedTerm Type
  deriving (Show)

type Context = [(Name, Type)]
type Sequent = (Context, TypedTerm)

annotate :: Context -> Term -> Either String Sequent
annotate ctx term = do
    (ctx',tt, _) <- annotateN 0 ctx term
    return (ctx',tt)

-- k: number of `new` named so far. It is threaded separately from the context
-- because a lambda discards the context of its body.
annotateN :: Int -> Context -> Term -> Either String (Context,TypedTerm, Int)
annotateN k ctx term = case term of 

    -- 1. VALUES: 
    V v -> case v of 
        -- Checks variable (e.g. q1, f, x)
        Var x -> do
            (ty, newCtx) <- lookupAndConsume x ctx
            return (newCtx, TV (TVar x ty) ty, k)
        
        -- Lambdas, add to the context and enter the body 
        Lambda x tyArg body -> do
            (_, tBody, k1) <- annotateN k ((x, tyArg) : ctx) body
            let lamTy = TFun tyArg (getTType tBody)
            return (ctx, TV (TLambda x tyArg tBody lamTy) lamTy, k1)
        -- Pairs, find the typed terms, compute the type and return
        Tensor t1 t2 -> do
                    (ctx1, tt1, k1) <- annotateN k ctx t1
                    (ctx2, tt2, k2) <- annotateN k1 ctx1 t2
                    
                    let pairTy = TPair (getTType tt1) (getTType tt2)
                    
                    return (ctx2, TV (TTensor tt1 tt2 pairTy) pairTy, k2)
    -- 2. GATE: Checks the arguments in sequence, CNOT has two but it scales (might be useful)
    Gate name args -> do
        -- Recursively covers args :: Term structure (avoiding annotateList)
        (ctxAfterArgs, revArgs, k1) <- foldM checkArg (ctx, [], k) args
        -- reverse so it mantains order of appearence 
        let tArgs = reverse revArgs               
            argTypes = map getTType tArgs
        -- Checks whether the gate exists and which types it returns
        retTy <- checkGate name argTypes 
        let finalArgs = case (name, tArgs) of
                ("CNOT", [arg1, arg2]) -> 
                    let pairTy = TPair (getTType arg1) (getTType arg2)
                    in [TV (TTensor arg1 arg2 pairTy) pairTy]
                _ -> tArgs
        return (ctxAfterArgs, TGate name finalArgs retTy, k1)
      where
        checkArg (c, acc, n) t = do
            (c', tt, n') <- annotateN n c t
            return (c', tt : acc, n')

    -- 3. LET: Introduces x, checks the body, then removes it
    Let x ty val body -> do
        (ctx1, tVal, k1) <- annotateN k ctx val
        -- Add x to the context to check the body
        (ctx2, tBody, k2) <- annotateN k1 ((x, ty) : ctx1) body
        -- Verify that x has been consumed (optional, depends on the linear logic)
        if any ((== x) . fst) ctx2
            then Left $ "Error: the linear variable '" ++ x ++ "' must be consumed in the body."
            else return (ctx2, TLet x ty tVal tBody (getTType tBody), k2)

    -- 4. DECOMP: Unpacks a pair <x,y>
    Decomp x y t1 t2 -> do
        (ctx1, tt1, k1) <- annotateN k ctx t1
        case getTType tt1 of
            TPair tx ty -> do
                -- Add x and y to the context
                (ctx2, tt2, k2) <- annotateN k1 ((x, tx) : (y, ty) : ctx1) t2
                -- Cleanup: x and y must not escape the Decomp
                let finalCtx = filter (\(n,_) -> n /= x && n /= y) ctx2
                return (finalCtx, TDecomp x y tt1 tt2 (getTType tt2), k2)
            _ -> Left "Decomp requires a Pair type."

    -- 5. APPLICATION: f(x)
    App f arg -> do
        (ctx1, tf, k1) <- annotateN k ctx f
        (ctx2, tArg, k2) <- annotateN k1 ctx1 arg
        case getTType tf of
            TFun tIn tOut | tIn == getTType tArg -> 
                Right (ctx2, TApp tf tArg tOut, k2)
            _ -> Left "Type mismatch in function application."


    --6 IF
    If cond termThen termElse -> do
        (ctx1, tCond, k1) <- annotateN k ctx cond 
        if getTType tCond /= TBit -- Bit only (measures only)
            then Left "Error: The If conditional must be a bit."
            else do
                -- Analyse THEN
                (ctxThen, tThen, k2) <- annotateN k1 ctx1 termThen
                
                -- Analyse ELSE
                (ctxElse, tElse, k3) <- annotateN k2 ctx1 termElse
                
                -- The contexts must be equal (otherwise inconsistent with what is executed afterwards)
                -- To be considered a patch for now
                if ctxThen /= ctxElse
                    then Left "Linearity error: the two branches of the If consume different resources."
                    else do
                        -- 5. The type of the IF is the type of the branches (which must be the same for similar reasons)
                        let tyThen = getTType tThen
                        let tyElse = getTType tElse
                        if tyThen /= tyElse
                            then Left "Error: the branches of the If return different types."
                            else return (ctxThen, TIf tCond tThen tElse tyThen, k3)
    --7New
    New n -> 
      if n == 0 || n == 1
        then 
          let -- Generates a unique name from the counter
              varId = "new_" ++ show n ++ "_" ++ show (k + 1)
          in Right (ctx, TNew n varId TQbit, k + 1)
        else Left $ "Type error: 'new' accepts only 0 or 1, received: " ++ show n


--For example in Values it consumes the symbol
lookupAndConsume :: Name -> Context -> Either String (Type, Context)
lookupAndConsume x [] = Left $ "Linearity error: variabile '" ++ x ++ "' not found or already used."
lookupAndConsume x ((n, t):xs)
  | x == n    = Right (t, xs) 
  | otherwise = do
      (t', rest) <- lookupAndConsume x xs
      Right (t', (n, t) : rest)



--Returns the type
getTType :: TypedTerm -> Type
getTType term = case term of 
    (TV _ t)            -> t
    (TNew _ _ t)        -> t
    (TApp _ _ t)        -> t
    (TGate _ _ t)       -> t
    (TLet _ _ _ _ t)    -> t
    (TDecomp _ _ _ _ t) -> t
    (TIf _ _ _ t)       -> t
    x                   -> error $ "Missing pattern in getTType:" ++ show x

-- Gates type-check
checkGate :: String -> [Type] -> Either String Type
checkGate name args = case (name, args) of
    ("H", [TQbit])    -> Right TQbit
    ("X", [TQbit])    -> Right TQbit
    ("Z", [TQbit])    -> Right TQbit
    ("T", [TQbit])    -> Right TQbit
    ("Y", [TQbit])    -> Right TQbit
    ("CNOT", [TQbit, TQbit]) -> Right (TPair TQbit TQbit)
    ("M", [TQbit])    -> Right TBit -- Meas :: Qbit --o Bit
    ("CNOT", _) -> Left "CNOT richiede esattamente due argomenti di tipo Qbit."
    (n, _)      -> Left $ "Gate sconosciuto o argomenti errati per: " ++ n


