### An overview of the changes respecting the original project

### Main.hs

Was mainly let the same, changed a little the way of printing the circuit. Parser was not changed, only changed that in tuples always it was structured (ctx,term) as appeared on a derivation.

### CreateDerivation.hs, Lexer.hs, TypeTree.hs

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
