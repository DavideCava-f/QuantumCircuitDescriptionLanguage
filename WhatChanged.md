### An overview of the changes respecting the original project

### Main.hs

Was mainly let the same, changed a little the way of printing the circuit. Parser was not changed, only changed that in tuples always it was structured (ctx,term) as appeared on a derivation.

### CreateDerivation.hs

I changed the program so now accepts any gate and in particular any arity, therefore in ``buildDerivationV`` and ``getGateType`` I made some changes so it accepts these modifications:

```hs
-- buildDerivationV before:
  TTensor t1 t2 tensorType ->   
    let t1Tree = buildDerivation prem t1
        t2Tree = buildDerivation prem t2
    in Node 
         { rootLabel = (prem, TV (TTensor t1 t2 tensorType) t, t)
         , subForest = [t1Tree, t2Tree] 
         }

-- buildDerivationV after:
  TTensor ts tensorType ->   
    Node 
      { rootLabel = (prem, TV (TTensor ts tensorType) t, t)
      , subForest = map (buildDerivation prem) ts
      }

-- getGateType before:
getGateType :: String -> Type
getGateType "CNOT" = 
  let qpair = TPair TQbit TQbit 
  in TFun qpair qpair                     -- (Q x Q) -> (Q x Q)
getGateType "M"      = TFun TQbit TBit
getGateType _      = TFun TQbit TQbit

-- getGateType after:
getGateType :: String -> [TypedTerm] -> Type -> Type
getGateType _ [arg] t = TFun (getTType arg) t
getGateType g args  _ = error ("getGateType: gate " ++ g ++ " with " ++ show (length args) ++ " arguments")
```

### TypeTree.hs

Here it's probably where most of the changes regarding accepting any gate with any arity are. Firstly when defining the data:

```hs
-- before:
data Type = TBit | TQbit | TFun Type Type | TPair Type Type
  deriving (Eq, Show)

-- after:
data Type = TBit | TQbit | TFun Type Type | TPair [Type]   
  deriving (Eq, Show)

-- before:
data Term 
  = App Term Term -- H(x) CNOT(x,y) 
  | Let Name Type Term Term -- let x = q1 in H(x)
  | Decomp Name Name Term Term  -- let <x,y> = t in t
  | If Term Term Term
  | New Int
  | Gate String [Term]          -- Per U(v1...vn) come H, X, CNOT
  | V Value
  deriving (Show)

-- after:
data Term 
  = App Term Term                       -- H(x) CNOT(x,y) 
  | Let Name Type Term Term             -- let x = q1 in H(x)
  | Decomp [Name] Term Term             -- let <x1,...,xn> = t in t
  | If Term Term Term
  | New Int
  | Gate Name [Term]                  -- U(v1...vn) such as H, X, CNOT
  | V Value
  deriving (Show)

-- before:
data Value
  = Var Name
  | Lambda Name Type Term
  | Tensor Term Term
  deriving (Show)

-- after:
data Value
  = Var Name
  | Lambda Name Type Term
  | Tensor [Term]                       
  deriving (Show)

-- before:
data TypedValue
  = TVar Name Type
  | TLambda Name Type TypedTerm Type  -- Lambda: arg, tipo_arg, corpo_tipizzato, tipo_totale
  | TTensor TypedTerm TypedTerm Type    -- Coppia di due termini tipizzati
  deriving (Show)

--after:
data TypedValue
  = TVar Name Type
  | TLambda Name Type TypedTerm Type    
  | TTensor [TypedTerm] Type            
  deriving (Show)

-- before:
data TypedTerm
  = TV TypedValue Type
  | TApp TypedTerm TypedTerm Type
  | TNew Int String Type
  | TGate String [TypedTerm] Type
  | TLet Name Type TypedTerm TypedTerm Type
  | TDecomp Name Name TypedTerm TypedTerm Type
  | TIf TypedTerm TypedTerm TypedTerm Type
  deriving (Show)

-- after:
data TypedTerm
  = TV TypedValue Type
  | TApp TypedTerm TypedTerm Type
  | TNew Int String Type
  | TGate String [TypedTerm] Type
  | TLet Name Type TypedTerm TypedTerm Type
  | TDecomp [Name] TypedTerm TypedTerm Type
  | TIf TypedTerm TypedTerm TypedTerm Type
  deriving (Show)
```

Secondly in the function ```annoateN```:

```hs
-- before:
        Tensor t1 t2 -> do
                    (tt1, ctx1) <- annotate ctx t1
                    (tt2, ctx2) <- annotate ctx1 t2
                    
                    let pairTy = TPair (getTType tt1) (getTType tt2)
                    
                    return (TV (TTensor tt1 tt2 pairTy) pairTy, ctx2)
-- after:
        Tensor ts -> do
            (ctxN, revTts, kN) <- foldM checkArg (ctx, [], k) ts
            let tts    = reverse revTts
                pairTy = TPair (map getTType tts)
            return (ctxN, TV (TTensor tts pairTy) pairTy, kN)

-- before:
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
-- after:
    Gate name args -> do
        (ctxAfterArgs, revArgs, k1) <- foldM checkArg (ctx, [], k) args
        -- reverse so it mantains order of appearence 
        let tArgs = reverse revArgs               
            argTypes = map getTType tArgs
        -- Checks whether the gate exists and which types it returns
        retTy <- checkGate name argTypes 
        -- A gate with several inputs takes them as one tuple
        let finalArgs = case tArgs of
                []  -> []
                [a] -> [a]
                _   -> [TV (TTensor tArgs (TPair argTypes)) (TPair argTypes)]
        return (ctxAfterArgs, TGate name finalArgs retTy, k1)

```
Also added the function:
```hs
checkArg :: (Context, [TypedTerm], Int) -> Term -> Either String (Context, [TypedTerm], Int)
checkArg (c, acc, n) t = do
    (c', tt, n') <- annotateN n c t
    return (c', tt : acc, n')
```
And instead of stating acceptable gates by their name, just checks the structure:
```hs
-- before:
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

-- after:
checkGate :: String -> [Type] -> Either String Type
checkGate name args
    | any (/= TQbit) args =
        Left $ "Gate " ++ name ++ ": every argument must be a qbit, received " ++ show args
    | name == "M" = case args of
        [_] -> Right TBit
        _   -> Left $ "M measures exactly one qubit, received " ++ show (length args) ++ " arguments"
    | otherwise = Right (tupleOf args)
```

### Lexer.hs

No changes.

### CircuitGraph.hs

- Cleaned data and types

What is still the same:

```hs
data Position = L | R deriving (Show, Eq)
data Polarity = P | N deriving (Show, Eq)

emptyAddress :: Address
emptyAddress = Map.empty

data Rule
  = TLAMBDA Name
  | TGATE String [TypedTerm]
  | TTENSOR
  | TVAR Name
  | TAPP
  | TDECOMP Name Name
  | TLET Name
  | TIF
  deriving (Show)
```

What changed:

```hs
-- before
data PosInSeq = Concl | Prem String Int deriving (Show,Eq) =>
-- after
data Formula = InConcl | InPrem Name deriving (Eq)

-- before
data Label = Lab Int deriving (Show,Eq)  =>
-- after
newtype Label = Lab Int deriving (Show, Eq, Ord)

-- before
type Token = (Id, Label, Address) =>
-- after
data Token = Token
  { tokPos   :: Pos
  , tokLabel :: Label
  , tokAddr  :: Address
  } deriving (Show)

-- before
type TokenLabelList = [(Id, Label)] =>
-- after
type TokenLabelList = [(Pos, Label)]

-- before
type Id = (TypedTerm, Polarity, [Position], PosInSeq, [PosInPi]) =>
-- after
data Pos = Pos
  { posNode :: Zipper
  , posOcc  :: Occ
  } deriving (Eq, Show)

-- before
type Circuit = [CablePair] =>
-- after
type Circuit = [Gate]

-- before
type CablePair = (Cable, Int)
data Cable 
  = LabelH Label Label Id
  | LabelX Label Label Id
  | LabelY Label Label Id
  | LabelZ Label Label Id
  | LabelT Label Label Id
  | LabelM Label Label Id
  | LabelI Label Label
  | LabelCNOT Label Label Id 
  deriving (Show) =>
-- after
data Gate
  = Wire String Label Label           -- I, H, X, Y, Z, T, M   (labelIn, labelOut)
  | CNOT Label Label Label Label      -- inControl, outControl, inTarget, outTarget
  deriving (Show, Eq)
```

What was added:

```hs
data Formula = InConcl | InPrem Name deriving (Eq)

instance Show Formula where
  show InConcl    = "Concl"
  show (InPrem x) = "Prem " ++ show x

data Occ = Occ
  { occFormula :: Formula
  , occPath    :: [Position]
  , occPol     :: Polarity
  } deriving (Eq, Show)

data ExtCircuit
  = Leaf Circuit
  | Branch Circuit Label ExtCircuit ExtCircuit
  deriving (Show)
```

What was deleted:

```hs
data TransformedGate
  = SingleGate String Label Label   -- Es. H, X, Y, Z, T con (LabelIn, LabelOut)
  | FullCNOT Label Label Label Label -- (ControlIn, ControlOut, TargetIn, TargetOut)
  | GateI Label Label                -- Gate Identità
  deriving (Show, Eq)

type FinalCircuit = [TransformedGate]

type DATA = [Id]

data TokenState = TokenState
  { tokens   :: [Token]
  , lastLabel :: Int
  } deriving (Show)
```

- Changed the way of travelling the derivation tree

I no longer use DATA, instead I implemented in ``DerivationZipper.hs`` a Zipper with functions to move and access the zipper in order to travel the tree better.

For more information on the Zipper structure, I recommend the [HaskellWiki page for Zipper](https://wiki.haskell.org/index.php?title=Zipper).

- ``startMachine``

Now when recieving the **TypeDerivation** it turns it into a **Zipper** with the ``fromTree function``. ``findInitials`` and ``addTokens`` are the same.

- ``runMachine``

Still uses stopCond to check if a token has arrived to the conclusion with positive polarity, but instead of letting one token travel until finishing and then the other, advances one step per token (``stepToken``). Also now checks if a token is waiting on a CNOT so it does not advance and stays waiting for the other one to arrive.

```hs
go (done, c) t
        | stopCond (tokPos t) || isJust (inCNOT t) = Right (done ++ [t], c)
        | otherwise = do
            (t', c') <- stepToken t c
            return (done ++ [t'], c')
```

There's a loop ensuring all tokens move one step at a time at their given turn and checking that if from one state to another nothing changes there might be a deadlock.

```hs
      loop toks c
        | all (stopCond . tokPos) toks = Right (toks, c)
        | otherwise = do
            (toks1, c1) <- foldM go ([], c) toks
            (toks2, c2) <- fireAllCNOT toks1 c1
            if map tokPos toks2 == map tokPos toks
              then Left ("Deadlock: tokens waiting at " ++ show
                         [ showPos (tokPos t) | t <- toks2, not (stopCond (tokPos t)) ])
              else loop toks2 c2
```

Also in every turn, I check if there are tokens waiting at a CNOT so the CNOT can be added to the circuit (if not, it continues without doing anything). 

- ``stepToken``

Applies the rule for that token accordingly, distinguishing between rules like application, tensor, descomposition, etc and being on a Gate axiom (in which the gate is applied, except if is a CNOT which is take cared apart).

- ``fireAllCNOT``

Checks if there are tokens waiting on a CNOT (``readyCNOT``) and if there are, it returns the position of these tokens in the token list in format (i,j) where i is the control token and j the target. Once having the positions, calls ``fireCNOT`` for these exact positions which calls applyRule for the CNOT gate, advances two positions at one time (for both tokens to continue travelling) and adds the gate to the circuit. Then it changes the toks list so the tokens just processed are updated and calls ``fireAllCNOT`` again to check weather there are more tokens waiting.
