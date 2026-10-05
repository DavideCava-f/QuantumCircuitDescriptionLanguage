module CircuitGraph where

import Control.Monad (foldM)
import qualified Data.Map as Map
import TypeTree (Type(..), Name, TypedTerm(..), TypedValue(..))
import CreateDerivation (Concl, TypeDerivation, Tree(..))
import DerivationZipper
import Data.Maybe (listToMaybe)


data Position = L | R deriving (Show, Eq)
data Polarity = P | N deriving (Show, Eq)
newtype Label = Lab Int deriving (Show, Eq, Ord)

data Formula = InConcl | InPrem Name deriving (Eq)

instance Show Formula where
  show InConcl    = "Concl"
  show (InPrem x) = "Prem " ++ show x

data Occ = Occ
  { occFormula :: Formula
  , occPath    :: [Position]
  , occPol     :: Polarity
  } deriving (Eq, Show)

data Pos = Pos
  { posNode :: Zipper
  , posOcc  :: Occ
  } deriving (Eq, Show)

type Address = Map.Map Label Bool

emptyAddress :: Address
emptyAddress = Map.empty

data Token = Token
  { tokPos   :: Pos
  , tokLabel :: Label
  , tokAddr  :: Address
  } deriving (Show)

type TokenLabelList = [(Pos, Label)]

-- Circuit: sequence of gates c^l_r. A CNOT is emitted in halves (one wire per
-- token) and the two halves are joined in buildFinalCircuit: they are the same
-- gate if they have the same gate axiom (pathOf) and opposite L/R sides.
type GateRef = [Int]

data Cable
  = Wire String Label Label                 -- I, H, X, Y, Z, T, M  (labelIn, labelOut)
  | HalfCNOT Position GateRef Label Label   -- side (L = control, R = target), axiom, in, out
  deriving (Show, Eq)

type Circuit = [Cable]

-- E ::= C | C -l-> (E, F)
data ExtCircuit
  = Leaf Circuit
  | Branch Circuit Label ExtCircuit ExtCircuit
  deriving (Show)

-- E@G
at :: Address -> ExtCircuit -> Maybe Circuit
at _ (Leaf c) = Just c
at g (Branch _ l e f) = case Map.lookup l g of
  Just True  -> at (Map.delete l g) e
  Just False -> at (Map.delete l g) f
  Nothing    -> Nothing

-- E@G[C]: applies a modification to the circuit at address G
modifyAt :: Address -> (Circuit -> Circuit) -> ExtCircuit -> ExtCircuit
modifyAt _ h (Leaf c) = Leaf (h c)
modifyAt g h (Branch c l e f) = case Map.lookup l g of
  Just True  -> Branch c l (modifyAt (Map.delete l g) h e) f
  Just False -> Branch c l e (modifyAt (Map.delete l g) h f)
  Nothing    -> Branch c l e f

data TransformedGate
  = SingleGate String Label Label     -- E.g. H, X, Y, Z, T, M, I with (LabelIn, LabelOut)
  | FullCNOT Label Label Label Label  -- (InControl, OutControl, InTarget, OutTarget)
  deriving (Show, Eq)

type FinalCircuit = [TransformedGate]

-- Configuration C = (pi, M, E)
data Config = Config
  { cfgTokens    :: [Token]      -- M
  , cfgCircuit   :: ExtCircuit   -- E
  , cfgNextLabel :: Int          -- last label used (generator of lab(-))
  } deriving (Show)

emptyConfig :: Config
emptyConfig = Config [] (Leaf []) 0

-- Typing rule the token is crossing
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

------------------------------------------------------------------------------
-- Positions: construction and polarity
------------------------------------------------------------------------------

flipPol :: Polarity -> Polarity
flipPol P = N
flipPol N = P

isBase :: Type -> Bool
isBase TQbit = True
isBase TBit  = True
isBase _     = False

-- Polarity of the occurrence `path` in the formula `f` of the judgement: start from
-- P for the conclusion and from N for the premises, flip on the left of an
-- arrow, do not flip under the tensor. Nothing if the path does not
-- correspond to a base type of the formula.
polarityAt :: Concl -> Formula -> [Position] -> Maybe Polarity
polarityAt (prem, _, ty) f path = case f of
    InConcl  -> walk P ty path
    InPrem x -> Map.lookup x prem >>= \t -> walk N t path
  where
    walk pol t            []       = if isBase t then Just pol else Nothing
    walk pol (TFun a _)   (L : ps) = walk (flipPol pol) a ps
    walk pol (TFun _ b)   (R : ps) = walk pol b ps
    walk pol (TPair a _)  (L : ps) = walk pol a ps
    walk pol (TPair _ b)  (R : ps) = walk pol b ps
    walk _   _            _        = Nothing

-- Builds the target position of a rule (instead of searching for it in a list).
mkPos :: Zipper -> Formula -> [Position] -> Maybe Pos
mkPos z f p = Pos z . Occ f p <$> polarityAt (judgement z) f p

-- All the positions of a judgement (premises first, in name order,
-- then the conclusion; this is the order of the old processJudgment).
positionsOf :: Zipper -> [Occ]
positionsOf z =
  let (prem, _, ty) = judgement z
      inType f start t = map (uncurry (Occ f)) (walk start [] t)
      walk pol acc t | isBase t = [(reverse acc, pol)]
      walk pol acc (TFun a b)   = walk (flipPol pol) (L : acc) a ++ walk pol (R : acc) b
      walk pol acc (TPair a b)  = walk pol (L : acc) a ++ walk pol (R : acc) b
      walk _   _   _            = []
  in concat [ inType (InPrem x) N t | (x, t) <- Map.toList prem ] ++ inType InConcl P ty

-- The token has reached a positive position of the conclusion of pi (PDATA)
stopCond :: Pos -> Bool
stopCond (Pos z occ) = isRoot z && occPol occ == P

tokenAtNode :: Pos -> [Token] -> Int
tokenAtNode pos = foldr (\t rec -> if (posNode . tokPos) t == posNode pos then 1+rec else rec) 0 


-- True if the judgement is the axiom of a gate  |- c : T(c)
isGateAxiom :: Zipper -> Bool
isGateAxiom z = case termOf z of
  TGate _ [] _ -> True
  _            -> False

------------------------------------------------------------------------------
-- Rule inference
--
-- A negative token goes up towards the premises: the rule is that of the judgement
-- it lies in. A positive token goes down through the rule below: the
-- rule is that of the parent judgement.
------------------------------------------------------------------------------

ruleAt :: Pos -> Either String Rule
ruleAt (Pos z occ) = do
  node <- maybe (Left ("nessuna regola sotto la radice per " ++ showPos (Pos z occ))) Right
                (if occPol occ == P then up z else Just z)
  case termOf node of
    TV (TVar x _) _        -> Right (TVAR x)
    TV (TLambda x _ _ _) _ -> Right (TLAMBDA x)
    TV (TTensor _ _ _) _   -> Right TTENSOR
    TDecomp a b _ _ _      -> Right (TDECOMP a b)
    TApp _ _ _             -> Right TAPP
    TLet x _ _ _ _         -> Right (TLET x)
    TGate g args _         -> Right (TGATE g args)
    TIf _ _ _ _            -> Right TIF
    TNew _ _ _             -> Left ("Termine non riconosciuto (new) in " ++ showPos (Pos z occ))

------------------------------------------------------------------------------
-- Structural rules (Fig. 6a of the paper)
--
-- Each clause is a navigation in the zipper (up / down i / sibling j)
-- followed by mkPos, which builds the target occurrence and recomputes its
-- polarity. `z` is always the judgement the token lies in.
------------------------------------------------------------------------------

-- Premise that owns the variable y among the children k
ownerOf :: Name -> [Int] -> Zipper -> Maybe Zipper
ownerOf _ [] _ = Nothing
ownerOf y (k : ks) z = case down k z of
  Just zk | Map.member y (premOf zk) -> Just zk
  _                                  -> ownerOf y ks z

-- Gate node: children [gate axiom (0), argument (1)]
applyGate :: Pos -> Maybe Pos
applyGate (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> down 1 z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> up z     >>= \z' -> mkPos z' (InPrem y) path
  (InConcl, P) -> case childIndex z of
      -- Leaving the axiom's output (R : p): go to the conclusion of the gate node
      Just 0 -> up z >>= \z' -> mkPos z' InConcl (drop 1 path)
      -- Leaving the argument's conclusion: enter the axiom's input (L : p)
      _      -> sibling 0 z >>= \z' -> mkPos z' InConcl (L : path)
  (InConcl, N)
      -- Circuit rule: from the input to the output of the axiom
      | isGateAxiom z -> mkPos z InConcl (R : drop 1 path)
      -- Negative position in the gate's output: go down into the axiom
      | otherwise     -> down 0 z >>= \z' -> mkPos z' InConcl (R : path)

-- Application node: children [function (0), argument (1)]
applyApp :: Pos -> Maybe Pos
applyApp (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> ownerOf y [0, 1] z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> up z >>= \z' -> mkPos z' (InPrem y) path
  -- B- of the conclusion  ->  B- inside A -o B of the function
  (InConcl, N)  -> down 0 z >>= \z' -> mkPos z' InConcl (R : path)
  (InConcl, P)  -> case (childIndex z, path) of
      -- leaving the function on the output B+  ->  B+ of the conclusion
      (Just 0, R : ps) -> up z >>= \z' -> mkPos z' InConcl ps
      -- leaving the function on the input A-  ->  A of the argument's conclusion
      (Just 0, L : ps) -> sibling 1 z >>= \z' -> mkPos z' InConcl ps
      (Just 0, [])     -> Nothing
      -- leaving the argument A+  ->  A- inside A -o B of the function
      _                -> sibling 0 z >>= \z' -> mkPos z' InConcl (L : path)

-- Axiom x : A |- x : A: the token crosses the axiom
applyVar :: Name -> Pos -> Maybe Pos
applyVar x (Pos z (Occ f path pol)) = case (f, pol) of
  (InConcl, N)  -> mkPos z (InPrem x) path
  (InPrem _, N) -> mkPos z InConcl path
  _             -> Nothing

-- Pair node: children [t1 (0), t2 (1)]
applyTensor :: Pos -> Maybe Pos
applyTensor (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> ownerOf y [0, 1] z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> up z >>= \z' -> mkPos z' (InPrem y) path
  (InConcl, N)  -> case path of
      L : ps -> down 0 z >>= \z' -> mkPos z' InConcl ps
      R : ps -> down 1 z >>= \z' -> mkPos z' InConcl ps
      []     -> Nothing
  (InConcl, P)  -> do
      k  <- childIndex z
      z' <- up z
      mkPos z' InConcl ((if k == 0 then L else R) : path)

-- Node let <x,y> = M in N: children [pair (0), body (1)]
applyDecomp :: Name -> Name -> Pos -> Maybe Pos
applyDecomp x y (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem v, N) -> ownerOf v [0, 1] z >>= \z' -> mkPos z' (InPrem v) path
  (InPrem v, P)
      | v == x    -> sibling 0 z >>= \z' -> mkPos z' InConcl (L : path)
      | v == y    -> sibling 0 z >>= \z' -> mkPos z' InConcl (R : path)
      | otherwise -> up z >>= \z' -> mkPos z' (InPrem v) path
  (InConcl, N)  -> down 1 z >>= \z' -> mkPos z' InConcl path
  (InConcl, P)  -> case (childIndex z, path) of
      -- leaving the pair: left component -> x, right -> y in the body
      (Just 0, L : ps) -> sibling 1 z >>= \z' -> mkPos z' (InPrem x) ps
      (Just 0, R : ps) -> sibling 1 z >>= \z' -> mkPos z' (InPrem y) ps
      (Just 0, [])     -> Nothing
      -- leaving the body -> conclusion of the let
      _                -> up z >>= \z' -> mkPos z' InConcl path

-- Node lambda x: children [body (0)]
applyLambda :: Name -> Pos -> Maybe Pos
applyLambda x (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) | y /= x -> down 0 z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) | y /= x -> up z >>= \z' -> mkPos z' (InPrem y) path
  -- entering from the conclusion A -o B: A goes to the premise x, B to the body's conclusion
  (InConcl, N) -> case path of
      L : ps -> down 0 z >>= \z' -> mkPos z' (InPrem x) ps
      R : ps -> down 0 z >>= \z' -> mkPos z' InConcl ps
      []     -> Nothing
  -- leaving the body: the conclusion B goes right, the premise x goes left
  (InConcl, P)  -> up z >>= \z' -> mkPos z' InConcl (R : path)
  (InPrem _, P) -> up z >>= \z' -> mkPos z' InConcl (L : path)
  (InPrem _, N) -> Nothing

-- Node let x = M in N: children [value (0), body (1)]
applyLet :: Name -> Pos -> Maybe Pos
applyLet x (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) | y /= x -> ownerOf y [0, 1] z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) | y /= x -> up z >>= \z' -> mkPos z' (InPrem y) path
  -- x : A+ in the body  ->  A+ of the value's conclusion
  (InPrem _, P) -> sibling 0 z >>= \z' -> mkPos z' InConcl path
  (InPrem _, N) -> Nothing
  (InConcl, P)  -> case childIndex z of
      -- A+ of the value  ->  x : A in the body
      Just 0 -> sibling 1 z >>= \z' -> mkPos z' (InPrem x) path
      -- B+ of the body  ->  B+ of the let
      _      -> up z >>= \z' -> mkPos z' InConcl path
  -- B- of the let  ->  B- of the body
  (InConcl, N)  -> down 1 z >>= \z' -> mkPos z' InConcl path

applyRule :: Rule -> Pos -> Either String Pos
applyRule rule pos =
  let target = case rule of
        TLAMBDA x   -> applyLambda x pos
        TVAR x      -> applyVar x pos
        TAPP        -> applyApp pos
        TTENSOR     -> applyTensor pos
        TDECOMP x y -> applyDecomp x y pos
        TLET x      -> applyLet x pos
        TGATE _ _   -> applyGate pos
        TIF         -> Nothing
  in case (rule, target) of
       (TIF, _)       -> Left ("if-then-else non ancora supportato dalla macchina in " ++ showPos pos)
       (_, Just p)    -> Right p
       (_, Nothing)   -> Left ("Regola " ++ show rule ++ " non applicabile alla posizione " ++ showPos pos)

------------------------------------------------------------------------------
-- Circuit rule (Fig. 6b) and token travel
------------------------------------------------------------------------------

makeCable :: String -> Zipper -> [Position] -> Label -> Label -> Cable
makeCable "CNOT" axiom path lIn lOut = case path of
  (_ : side : _) -> HalfCNOT side (pathOf axiom) lIn lOut
  _              -> error ("Unexpected CNOT input position: " ++ show path)
makeCable g _ _ lIn lOut = Wire g lIn lOut

-- One machine step for a token
stepToken :: Token -> Config -> Either String (Token, Config)
stepToken tok cfg = do
  let pos@(Pos z occ) = tokPos tok
  rule <- ruleAt pos
  case rule of
    -- The token is on the input of a gate axiom: emits the gate
    TGATE g [] | occPol occ == N-> do
      pos' <- applyRule rule pos
      let n     = cfgNextLabel cfg + 1
          lOut  = Lab n
          cable = makeCable g z (occPath occ) (tokLabel tok) lOut
      return ( tok { tokPos = pos', tokLabel = lOut }
             , cfg { cfgCircuit = modifyAt (tokAddr tok) (++ [cable]) (cfgCircuit cfg)
                   , cfgNextLabel = n } )
    _ -> do
      pos' <- applyRule rule pos
      return (tok { tokPos = pos' }, cfg)

------------------------------------------------------------------------------
-- Initial configuration
------------------------------------------------------------------------------

-- Pre-order visit of all the judgements of pi
allNodes :: Zipper -> [Zipper]
allNodes z = z : concatMap allNodes [ zk | k <- [0 .. length (subForest (focus z)) - 1]
                                         , Just zk <- [down k z] ]

-- Initial positions: NDATA (negative positions of the conclusion of pi) and the
-- outputs of the `new` axioms, which play the role of the occurrences of * (ONES).
findInitials :: Zipper -> [Pos]
findInitials root =
  let ndata     = [ Pos root o | o <- positionsOf root, occPol o == N ]
      isNew z   = case termOf z of TNew _ _ _ -> True; _ -> False
      newTokens = [ Pos z o | z <- allNodes root, isNew z, o <- positionsOf z, occPol o == P ]
  in ndata ++ newTokens

-- Creates the initial tokens, one label each
addTokens :: [Pos] -> Config -> (Config, TokenLabelList)
addTokens poss cfg =
  let start  = cfgNextLabel cfg
      toks   = zipWith (\p i -> Token p (Lab i) emptyAddress) poss [start + 1 ..]
      assoc  = [ (tokPos t, tokLabel t) | t <- toks ]
  in (cfg { cfgTokens = cfgTokens cfg ++ toks, cfgNextLabel = start + length poss }, assoc)

-- Identity on every initial wire (the Id circuit of the initial configuration)
applyInitialIdentity :: Token -> Config -> (Token, Config)
applyInitialIdentity tok cfg =
  let n    = cfgNextLabel cfg + 1
      lOut = Lab n
      tok' = tok { tokLabel = lOut }
      cfg' = cfg { cfgCircuit = modifyAt (tokAddr tok) (++ [Wire "I" (tokLabel tok) lOut]) (cfgCircuit cfg)
                 , cfgNextLabel = n }
  in (tok', cfg')

setupInitialTokens :: Config -> Config
setupInitialTokens cfg =
  let (toks, cfg') = foldl (\(acc, c) t -> let (t', c') = applyInitialIdentity t c in (acc ++ [t'], c'))
                           ([], cfg) (cfgTokens cfg)
  in cfg' { cfgTokens = toks }

------------------------------------------------------------------------------
-- Machine
------------------------------------------------------------------------------

runMachine :: Config -> Either String (FinalCircuit, TokenLabelList)
runMachine cfg0 = do
  let cfg1 = setupInitialTokens cfg0

      -- one step for one token; finished tokens are left alone
      go toks (done, c) t
        | stopCond (tokPos t) = Right (done ++ [t], c)
        | Right (TGATE "CNOT" []) <- ruleAt (tokPos t)
        , occPol (posOcc (tokPos t)) == N
        , tokenAtNode (tokPos t) toks < 2 = Right (done ++ [t], c)
        | otherwise = do
            (t', c') <- stepToken t c
            return (done ++ [t'], c')

      -- one pass over all the tokens, then repeat unless the condition holds
      loop toks c
        | all (stopCond . tokPos) toks = Right (toks, c)
        | otherwise = do
            (toks', c') <- foldM (go toks) ([], c) toks
            if map tokPos toks' == map tokPos toks
              then Left ("Deadlock: tokens waiting at " ++ show 
              [showPos (tokPos t) | t <- toks', not(stopCond(tokPos t))])
              else loop toks' c'

  (finalToks, cfgEnd) <- loop (cfgTokens cfg1) cfg1 { cfgTokens = [] }
  circuit <- maybe (Left "Invalid empty address in the extended circuit")
                    Right (at emptyAddress (cfgCircuit cfgEnd))
  return (buildFinalCircuit circuit, [ (tokPos t, tokLabel t) | t <- finalToks ])


startMachine :: TypeDerivation -> Either String (FinalCircuit, TokenLabelList, TokenLabelList)
startMachine derivation = do
  let root             = fromTree derivation
      (cfg, assocList) = addTokens (findInitials root) emptyConfig
  (final, finalList) <- runMachine cfg
  return (final, assocList, finalList)

------------------------------------------------------------------------------
-- Final circuit: joining the two halves of each CNOT
------------------------------------------------------------------------------

findAndRemoveCNOT :: Position -> GateRef -> Circuit -> Maybe (Label, Label, Circuit)
findAndRemoveCNOT _ _ [] = Nothing
findAndRemoveCNOT side ref (c : cs) = case c of
  HalfCNOT side2 ref2 lIn lOut | ref2 == ref && side2 /= side -> Just (lIn, lOut, cs)
  _ -> do
    (lIn, lOut, rest) <- findAndRemoveCNOT side ref cs
    return (lIn, lOut, c : rest)

buildFinalCircuit :: Circuit -> FinalCircuit
buildFinalCircuit [] = []
buildFinalCircuit (c : rest) = case c of
  Wire g lIn lOut -> SingleGate g lIn lOut : buildFinalCircuit rest
  HalfCNOT side ref lIn1 lOut1 -> case findAndRemoveCNOT side ref rest of
    Just (lIn2, lOut2, remaining) ->
      let gate = case side of
            L -> FullCNOT lIn1 lOut1 lIn2 lOut2   -- this half is the control
            R -> FullCNOT lIn2 lOut2 lIn1 lOut1   -- this half is the target
      in gate : buildFinalCircuit remaining
    Nothing -> error ("Errore: CNOT DEVE avere la sua parte L o R: " ++ show ref)

------------------------------------------------------------------------------
-- Pretty
------------------------------------------------------------------------------

showPos :: Pos -> String
showPos (Pos z (Occ f path pol)) =
  "(" ++ show pol ++ ", " ++ show path ++ ", " ++ show f ++ ", " ++ show (pathOf z) ++ ")"

prettyPos :: Pos -> String
prettyPos (Pos z (Occ f path pol)) =
  let polStr = case pol of P -> "(+)"; N -> "(-)"
      lrStr  = if null path then "ε" else foldr1 (\a b -> a ++ "." ++ b) (map show path)
  in concat [ "[", show f, " | π", show (pathOf z), "] ", polStr, " LR: ", lrStr
            , "  ==>  ", show (termOf z) ]

prettyPrintPositions :: String -> [Pos] -> IO ()
prettyPrintPositions label ps = do
  putStrLn $ "\n" ++ replicate 10 '=' ++ " " ++ label ++ " (" ++ show (length ps) ++ " elementi) " ++ replicate 10 '='
  mapM_ (\(i, p) -> putStrLn $ show (i :: Int) ++ ". " ++ prettyPos p) (zip [1 ..] ps)

prettyPrintTokens :: String -> [Token] -> IO ()
prettyPrintTokens title [] = putStrLn $ "=== " ++ title ++ " (Vuoto) ==="
prettyPrintTokens title toks = do
  putStrLn $ "\n=== " ++ title ++ " (" ++ show (length toks) ++ " token) ==="
  mapM_ (\t -> putStrLn $ "TOKEN | Lab: " ++ show (tokLabel t)
                       ++ " | Addr: " ++ show (tokAddr t)
                       ++ " | " ++ prettyPos (tokPos t)) toks

formatTokenAssoc :: (Pos, Label) -> String
formatTokenAssoc (Pos z (Occ f path pol), lab) =
  "(" ++ show pol ++ ", " ++ show path ++ ", " ++ show f ++ ", " ++ show (pathOf z) ++ ", " ++ show lab ++ ")"

prettyPrintLists :: TokenLabelList -> TokenLabelList -> String
prettyPrintLists (s : xs) (e : ys) =
  formatTokenAssoc s ++ " ends in " ++ formatTokenAssoc e ++ "\n" ++ prettyPrintLists xs ys
prettyPrintLists _ _ = ""

prettyPrintAssocList :: TokenLabelList -> TokenLabelList -> IO ()
prettyPrintAssocList initList endingList = putStrLn (prettyPrintLists initList endingList)