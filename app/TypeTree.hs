module TypeTree where

import Data.List (sort)

type Name = String

data Type = TBit | TQbit | TFun Type Type | TPair Type Type
  deriving (Eq, Show)

data Term 
  = App Term Term -- H(x) CNOT(x,y) 
  | Let Name Type Term Term -- let x = q1 in H(x)
  | Decomp Name Name Term Term  -- let <x,y> = t in t
  | If Term Term Term
  | Gate String [Term]          -- Per U(v1...vn) come H, X, CNOT
  | V Value
  deriving (Show)


data Value
  = Var Name
  | Lambda Name Type Term
  | Tensor Term Term
  deriving (Show)

data TypedValue
  = TVar Name Type
  | TLambda Name Type TypedTerm Type  -- Lambda: arg, tipo_arg, corpo_tipizzato, tipo_totale
  | TTensor TypedTerm TypedTerm Type    -- Coppia di due termini tipizzati
  deriving (Show)

data TypedTerm
  = TV TypedValue Type
  | TApp TypedTerm TypedTerm Type
  | TGate String [TypedTerm] Type
  | TLet Name Type TypedTerm TypedTerm Type
  | TDecomp Name Name TypedTerm TypedTerm Type
  | TIf TypedTerm TypedTerm TypedTerm Type
  deriving (Show)


annotate :: Context -> Term -> Either String (TypedTerm, Context)
annotate ctx term = case term of

    -- 1. VALUES: 
    V v -> case v of --Se var la consuma(Linearita')
        -- Controlla variabile (es. q1, f, x)
        Var x -> do
            (ty, newCtx) <- lookupAndConsume x ctx
            return (TV (TVar x ty) ty, newCtx)
        
        -- Lambdas, aggiungo al contesto e entro nel body 
        Lambda x tyArg body -> do
            (tBody, _) <- annotate ((x, tyArg) : ctx) body
            let lamTy = TFun tyArg (getTType tBody)
            return (TV (TLambda x tyArg tBody lamTy) lamTy, ctx)
        -- Pairs, trovo i typed terms calcolo il tipo e ritorno
        Tensor t1 t2 -> do
                    (tt1, ctx1) <- annotate ctx t1
                    (tt2, ctx2) <- annotate ctx1 t2
                    
                    let pairTy = TPair (getTType tt1) (getTType tt2)
                    
                    return (TV (TTensor tt1 tt2 pairTy) pairTy, ctx2)
    -- 2. GATE: Esegue il controllo sugli argomenti in sequenza, il CNOT ne ha due ma scalabile(forse puo' servire)
    Gate name args -> do
        -- Funzione helper che processa la lista di argomenti
        (tArgs, ctxAfterArgs) <- annotateList ctx args
        let argTypes = map getTType tArgs
        -- Controlla se il gate esiste e che tipi restituisce
        retTy <- checkGate name argTypes 
        let finalArgs = case (name, tArgs) of
                ("CNOT", [arg1, arg2]) -> 
                    let pairTy = TPair (getTType arg1) (getTType arg2)
                    in [TV (TTensor arg1 arg2 pairTy) pairTy]
                _ -> tArgs
        return (TGate name finalArgs retTy, ctxAfterArgs)

    -- 3. LET: Introduce x, controlla il corpo, poi lo rimuove
    Let x ty val body -> do
        (tVal, ctx1) <- annotate ctx val
        -- Aggiungiamo x al contesto per il controllo del corpo
        (tBody, ctx2) <- annotate ((x, ty) : ctx1) body
        -- Verifichiamo che x sia stato consumato (opzionale, dipende dalla logica lineare)
        if any ((== x) . fst) ctx2
            then Left $ "Errore: la variabile lineare '" ++ x ++ "' deve essere consumata nel corpo."
            else return (TLet x ty tVal tBody (getTType tBody), ctx2)

    -- 4. DECOMP: Spacchetta una coppia <x,y>
    Decomp x y t1 t2 -> do
        (tt1, ctx1) <- annotate ctx t1
        case getTType tt1 of
            TPair tx ty -> do
                -- Aggiungiamo x e y al contesto
                (tt2, ctx2) <- annotate ((x, tx) : (y, ty) : ctx1) t2
                -- Pulizia: x e y non devono uscire dal Decomp
                let finalCtx = filter (\(n,_) -> n /= x && n /= y) ctx2
                return (TDecomp x y tt1 tt2 (getTType tt2), finalCtx)
            _ -> Left "Decomp richiede un tipo Pair."

    -- 5. APPLICAZIONE: f(x)
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
        if getTType tCond /= TBit -- Solo Bit(Solo measures)
            then Left "Errore: la condizione dell'IF deve essere un Bit."
            else do
                -- Analizzo THEN
                (tThen, ctxThen) <- annotate ctx1 termThen
                
                -- Analizzo ELSE
                (tElse, ctxElse) <- annotate ctx1 termElse
                
                -- I contesti devono essere uguali(altrimenti incoerenza con cio' che viene eseguito dopo)
                -- Per ora da considerarsi una patch
                if ctxThen /= ctxElse
                    then Left "Errore di linearità: i due rami dell'IF consumano risorse diverse."
                    else do
                        -- 5. Il tipo dell'IF e' il tipo dei rami (che deve essere lo stesso per motivi analoghi)
                        let tyThen = getTType tThen
                        let tyElse = getTType tElse
                        if tyThen /= tyElse
                            then Left "Errore: i rami dell'IF restituiscono tipi diversi."
                            else return (TIf tCond tThen tElse tyThen, ctxThen)

type Context = [(Name, Type)]

--Per esempio nei Values consuma il simbolo
lookupAndConsume :: Name -> Context -> Either String (Type, Context)
lookupAndConsume x [] = Left $ "Errore di linearità: variabile '" ++ x ++ "' non trovata o già usata."
lookupAndConsume x ((n, t):xs)
  | x == n    = Right (t, xs) 
  | otherwise = do
      (t', rest) <- lookupAndConsume x xs
      Right (t', (n, t) : rest)



--Per il CNOT, ha 2 argomenti
annotateList :: Context -> [Term] -> Either String ([TypedTerm], Context)
annotateList ctx [] = Right ([], ctx)
annotateList ctx (t:ts) = do
    (tt, ctx1) <- annotate ctx t
    (tts, ctx2) <- annotateList ctx1 ts
    return (tt:tts, ctx2)


--Ritorna il tipo
getTType :: TypedTerm -> Type
getTType (TV _ t) = t
getTType (TApp _ _ t) = t
getTType (TGate _ _ t) = t
getTType (TLet _ _ _ _ t) = t
getTType (TDecomp _ _ _ _ t) = t
getTType (TIf _ _ _ t) = t
getTType x = error $ "Pattern mancante in getTType: " ++ show x

--Controllo di tipi nei gates
checkGate :: String -> [Type] -> Either String Type
checkGate name args = case (name, args) of
    -- Gate a 1 Qubit
    ("H", [TQbit])    -> Right TQbit
    ("X", [TQbit])    -> Right TQbit
    ("Z", [TQbit])    -> Right TQbit
    ("T", [TQbit])    -> Right TQbit
    ("Y", [TQbit])    -> Right TQbit

    -- Gate a 2 Qubit (CNOT)
    ("CNOT", [TQbit, TQbit]) -> Right (TPair TQbit TQbit)

    -- Gate di Misura (trasforma Qbit in Bit classico)
    ("M", [TQbit])    -> Right TBit

    -- Errori comuni
    ("CNOT", _) -> Left "CNOT richiede esattamente due argomenti di tipo Qbit."
    (n, _)      -> Left $ "Gate sconosciuto o argomenti errati per: " ++ n


