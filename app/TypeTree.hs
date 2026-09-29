module TypeTree where

import Data.List (sort, isPrefixOf)

type Name = String

data Type = TBit | TQbit | TFun Type Type | TPair Type Type
  deriving (Eq, Show)

data Term 
  = App Term Term -- H(x) CNOT(x,y) 
  | Let Name Type Term Term -- let x = q1 in H(x)
  | Decomp Name Name Term Term  -- let <x,y> = t in t
  | If Term Term Term
  | New Int
  | Gate String [Term]          -- For U(v1...vn) such as H, X, CNOT
  | V Value
  deriving (Show)


data Value
  = Var Name
  | Lambda Name Type Term
  | Tensor Term Term
  deriving (Show)

data TypedValue
  = TVar Name Type
  | TLambda Name Type TypedTerm Type  -- Lambda: arg, arg_type, typed_body, total_type
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


annotate :: Context -> Term -> Either String (TypedTerm, Context)
annotate ctx term = case term of

    -- 1. VALUES: 
    V v -> case v of --If var, consumes it (Linearity)
        -- Checks variable (e.g. q1, f, x)
        Var x -> do
            (ty, newCtx) <- lookupAndConsume x ctx
            return (TV (TVar x ty) ty, newCtx)
        
        -- Lambdas, add to the context and enter the body 
        Lambda x tyArg body -> do
            (tBody, _) <- annotate ((x, tyArg) : ctx) body
            let lamTy = TFun tyArg (getTType tBody)
            return (TV (TLambda x tyArg tBody lamTy) lamTy, ctx)
        -- Pairs, find the typed terms, compute the type and return
        Tensor t1 t2 -> do
                    (tt1, ctx1) <- annotate ctx t1
                    (tt2, ctx2) <- annotate ctx1 t2
                    
                    let pairTy = TPair (getTType tt1) (getTType tt2)
                    
                    return (TV (TTensor tt1 tt2 pairTy) pairTy, ctx2)
    -- 2. GATE: Checks the arguments in sequence, CNOT has two but it scales (might be useful)
    Gate name args -> do
        -- Helper function that processes the argument list
        (tArgs, ctxAfterArgs) <- annotateList ctx args
        let argTypes = map getTType tArgs
        -- Checks whether the gate exists and which types it returns
        retTy <- checkGate name argTypes 
        let finalArgs = case (name, tArgs) of
                ("CNOT", [arg1, arg2]) -> 
                    let pairTy = TPair (getTType arg1) (getTType arg2)
                    in [TV (TTensor arg1 arg2 pairTy) pairTy]
                _ -> tArgs
        return (TGate name finalArgs retTy, ctxAfterArgs)

    -- 3. LET: Introduces x, checks the body, then removes it
    Let x ty val body -> do
        (tVal, ctx1) <- annotate ctx val
        -- Add x to the context to check the body
        (tBody, ctx2) <- annotate ((x, ty) : ctx1) body
        -- Verify that x has been consumed (optional, depends on the linear logic)
        if any ((== x) . fst) ctx2
            then Left $ "Errore: la variabile lineare '" ++ x ++ "' deve essere consumata nel corpo."
            else return (TLet x ty tVal tBody (getTType tBody), ctx2)

    -- 4. DECOMP: Unpacks a pair <x,y>
    Decomp x y t1 t2 -> do
        (tt1, ctx1) <- annotate ctx t1
        case getTType tt1 of
            TPair tx ty -> do
                -- Add x and y to the context
                (tt2, ctx2) <- annotate ((x, tx) : (y, ty) : ctx1) t2
                -- Cleanup: x and y must not escape the Decomp
                let finalCtx = filter (\(n,_) -> n /= x && n /= y) ctx2
                return (TDecomp x y tt1 tt2 (getTType tt2), finalCtx)
            _ -> Left "Decomp richiede un tipo Pair."

    -- 5. APPLICATION: f(x)
    App f arg -> do
        (tf, ctx1) <- annotate ctx f
        (tArg, ctx2) <- annotate ctx1 arg
        case getTType tf of
            TFun tIn tOut | tIn == getTType tArg -> 
                Right (TApp tf tArg tOut, ctx2)
            _ -> Left "Mismatch di tipi nell'applicazione della funzione."


    --6 IF
    If cond termThen termElse -> do
        (tCond, ctx1) <- annotate ctx cond 
        if getTType tCond /= TBit -- Bit only (measures only)
            then Left "Errore: la condizione dell'IF deve essere un Bit."
            else do
                -- Analyse THEN
                (tThen, ctxThen) <- annotate ctx1 termThen
                
                -- Analyse ELSE
                (tElse, ctxElse) <- annotate ctx1 termElse
                
                -- The contexts must be equal (otherwise inconsistent with what is executed afterwards)
                -- To be considered a patch for now
                if ctxThen /= ctxElse
                    then Left "Errore di linearità: i due rami dell'IF consumano risorse diverse."
                    else do
                        -- 5. The type of the IF is the type of the branches (which must be the same for similar reasons)
                        let tyThen = getTType tThen
                        let tyElse = getTType tElse
                        if tyThen /= tyElse
                            then Left "Errore: i rami dell'IF restituiscono tipi diversi."
                            else return (TIf tCond tThen tElse tyThen, ctxThen)
    --7New
    New n -> 
      if n == 0 || n == 1
        then 
          let -- Generates a unique name based on the variables already in the context
              count = length [ k | (k, _) <- ctx, "new_" `isPrefixOf` k ]
              varId = "new_" ++ show n ++ "_" ++ show (count + 1)
          in Right (TNew n varId TQbit, ctx)
        else Left $ "Errore di tipo: 'new' accetta solo 0 o 1, ricevuto: " ++ show n

type Context = [(Name, Type)]

--For example in Values it consumes the symbol
lookupAndConsume :: Name -> Context -> Either String (Type, Context)
lookupAndConsume x [] = Left $ "Errore di linearità: variabile '" ++ x ++ "' non trovata o già usata."
lookupAndConsume x ((n, t):xs)
  | x == n    = Right (t, xs) 
  | otherwise = do
      (t', rest) <- lookupAndConsume x xs
      Right (t', (n, t) : rest)



--For the CNOT, it has 2 arguments
annotateList :: Context -> [Term] -> Either String ([TypedTerm], Context)
annotateList ctx [] = Right ([], ctx)
annotateList ctx (t:ts) = do
    (tt, ctx1) <- annotate ctx t
    (tts, ctx2) <- annotateList ctx1 ts
    return (tt:tts, ctx2)


--Returns the type
getTType :: TypedTerm -> Type
getTType (TV _ t) = t
getTType (TNew _ _ t) = t
getTType (TApp _ _ t) = t
getTType (TGate _ _ t) = t
getTType (TLet _ _ _ _ t) = t
getTType (TDecomp _ _ _ _ t) = t
getTType (TIf _ _ _ t) = t
getTType x = error $ "Pattern mancante in getTType: " ++ show x

--Type checking of gates
checkGate :: String -> [Type] -> Either String Type
checkGate name args = case (name, args) of
    -- 1-Qubit gates
    ("H", [TQbit])    -> Right TQbit
    ("X", [TQbit])    -> Right TQbit
    ("Z", [TQbit])    -> Right TQbit
    ("T", [TQbit])    -> Right TQbit
    ("Y", [TQbit])    -> Right TQbit

    -- 2-Qubit gates (CNOT)
    ("CNOT", [TQbit, TQbit]) -> Right (TPair TQbit TQbit)

    -- Measurement gate (turns a Qbit into a classical Bit)
    ("M", [TQbit])    -> Right TBit

    -- Common errors
    ("CNOT", _) -> Left "CNOT richiede esattamente due argomenti di tipo Qbit."
    (n, _)      -> Left $ "Gate sconosciuto o argomenti errati per: " ++ n


