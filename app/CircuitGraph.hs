module CircuitGraph where

import Control.Monad (foldM)
import qualified Data.Map as Map
import TypeTree (Type(..), Name, TypedTerm(..), TypedValue(..))
import CreateDerivation (Concl, TypeDerivation, Tree(..))
import DerivationZipper
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import Data.List (elemIndex)


-- Step of a path inside a type: L/R = left/right of an arrow, C i = component i of a tuple
data Position = L | R | C Int deriving (Show, Eq)
data Polarity = P | N deriving (Show, Eq)
newtype Label = Lab Int deriving (Show, Eq, Ord)

data Formula = InConcl | InPrem Name
  deriving (Eq)

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

data Gate = Wire String Label Label | MultiGate String [Label] [Label] | Cond Label Circuit Circuit

  deriving (Show, Eq)

type Circuit = [Gate]

data ExtCircuit = Leaf Circuit | Branch Circuit Label ExtCircuit ExtCircuit
  deriving (Show)

-- E@G
at :: Address -> ExtCircuit -> Maybe Circuit
at _ (Leaf c) = Just c
at g (Branch _ l e f) = case Map.lookup l g of
  Just True  -> at (Map.delete l g) e
  Just False -> at (Map.delete l g) f
  Nothing    -> Nothing

modifyAt :: Address -> (Circuit -> Circuit) -> ExtCircuit -> ExtCircuit
modifyAt _ h (Leaf c) = Leaf (h c)
modifyAt g h (Branch c l e f) = case Map.lookup l g of
  Just True  -> Branch c l (modifyAt (Map.delete l g) h e) f
  Just False -> Branch c l e (modifyAt (Map.delete l g) h f)
  Nothing    -> Branch c l e f

-- Configuration C = (pi, M, E)
data Config = Config
  { cfgTokens    :: [Token]
  , cfgCircuit   :: ExtCircuit
  , cfgNextLabel :: Int
  } deriving (Show)

emptyConfig :: Config
emptyConfig = Config [] (Leaf []) 0

data Rule = TLAMBDA Name | TGATE String [TypedTerm] | TTENSOR | TVAR Name | TAPP
            | TDECOMP [Name] | TLET Name | TIF
  deriving (Show)


----- Utils -----


flipPol :: Polarity -> Polarity
flipPol P = N
flipPol N = P

isBase :: Type -> Bool
isBase TQbit = True
isBase TBit  = True
isBase _     = False

polarityAt :: Concl -> Formula -> [Position] -> Maybe Polarity
polarityAt (prem, _, ty) f path = case f of
    InConcl  -> walk P ty path
    InPrem x -> Map.lookup x prem >>= \t -> walk N t path
  where
    walk pol t            []       = if isBase t then Just pol else Nothing
    walk pol (TFun a _)   (L : ps) = walk (flipPol pol) a ps
    walk pol (TFun _ b)   (R : ps) = walk pol b ps
    walk pol (TPair ts)   (C i : ps) | i >= 0 && i < length ts = walk pol (ts !! i) ps
    walk _   _            _        = Nothing

mkPos :: Zipper -> Formula -> [Position] -> Maybe Pos
mkPos z f p = Pos z . Occ f p <$> polarityAt (judgement z) f p

positionsOf :: Zipper -> [Occ]
positionsOf z =
  let (prem, _, ty) = judgement z
      inType f start t = map (uncurry (Occ f)) (walk start [] t)
      walk pol acc t | isBase t = [(reverse acc, pol)]
      walk pol acc (TFun a b)   = walk (flipPol pol) (L : acc) a ++ walk pol (R : acc) b
      walk pol acc (TPair ts)   = concat [ walk pol (C i : acc) t | (i, t) <- zip [0 ..] ts ]
      walk _   _   _            = []
  in concat [ inType (InPrem x) N t | (x, t) <- Map.toList prem ] ++ inType InConcl P ty

stopCond :: Pos -> Bool
stopCond (Pos z occ) = isRoot z && occPol occ == P

isGateAxiom :: Zipper -> Bool
isGateAxiom z = case termOf z of
  TGate _ [] _ -> True
  _            -> False

-- True if the judgement is the axiom of a gate with several inputs: |- c : (A (x) B) -o C
isMultiGateAxiom :: Zipper -> Bool
isMultiGateAxiom z = case termOf z of
  TGate _ [] (TFun (TPair _) _) -> True
  _                               -> False

isIfRule :: Zipper -> Bool
isIfRule z = case termOf z of
  TIf _ _ _ _ -> True
  _           -> False

-- Just the axiom if the token is waiting on an input of a multi-input gate:
-- path L : p, with L = input of the arrow and p = which input inside the pair
inMultiGate :: Token -> Maybe Zipper
inMultiGate tok = case tokPos tok of
  Pos z (Occ InConcl (L : _) N) | isMultiGateAxiom z -> Just z
  _                                                  -> Nothing

waitingAtIf :: Token -> Maybe Zipper
waitingAtIf tok = case tokPos tok of
  Pos z (Occ InConcl _ N)    | isIfRule z                                          -> Just z
  Pos z (Occ InConcl [] P)   | childIndex z == Just 0, Just z' <- up z, isIfRule z'-> Just z'
  Pos z (Occ (InPrem y) _ N) | isIfRule z && isJust (ownerOf y [1,2] z)            -> Just z
  _                                                                                -> Nothing

-- Input positions of a gate axiom, in argument order
axiomInputs :: Zipper -> [Occ]
axiomInputs z = [ o | o <- positionsOf z, occFormula o == InConcl, occPol o == N ]

----- Application of rules, following the paper -----

ruleAt :: Pos -> Either String Rule
ruleAt (Pos z occ) = do
  node <- maybe (Left ("nessuna regola sotto la radice per " ++ showPos (Pos z occ))) Right
                (if occPol occ == P then up z else Just z)
  case termOf node of
    TV (TVar x _) _        -> Right (TVAR x)
    TV (TLambda x _ _ _) _ -> Right (TLAMBDA x)
    TV (TTensor _ _) _     -> Right TTENSOR
    TDecomp xs _ _ _       -> Right (TDECOMP xs)
    TApp {}             -> Right TAPP
    TLet x _ _ _ _         -> Right (TLET x)
    TGate g args _         -> Right (TGATE g args)
    TIf {}            -> Right TIF
    TNew {}             -> Left ("Termine non riconosciuto (new) in " ++ showPos (Pos z occ))


ownerOf :: Name -> [Int] -> Zipper -> Maybe Zipper
ownerOf _ [] _ = Nothing
ownerOf y (k : ks) z = case down k z of
  Just zk | Map.member y (premOf zk) -> Just zk
  _                                  -> ownerOf y ks z

applyGate :: Pos -> Maybe Pos
applyGate (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> down 1 z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> up z     >>= \z' -> mkPos z' (InPrem y) path
  (InConcl, P) -> case childIndex z of
      Just 0 -> up z >>= \z' -> mkPos z' InConcl (drop 1 path)
      _      -> sibling 0 z >>= \z' -> mkPos z' InConcl (L : path)
  (InConcl, N)
      -- Circuit rule:
      | isGateAxiom z -> mkPos z InConcl (R : drop 1 path)
      | otherwise     -> down 0 z >>= \z' -> mkPos z' InConcl (R : path)

applyApp :: Pos -> Maybe Pos
applyApp (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> ownerOf y [0, 1] z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> up z >>= \z' -> mkPos z' (InPrem y) path
  (InConcl, N)  -> down 0 z >>= \z' -> mkPos z' InConcl (R : path)
  (InConcl, P)  -> case (childIndex z, path) of
      (Just 0, R : ps) -> up z >>= \z' -> mkPos z' InConcl ps
      (Just 0, L : ps) -> sibling 1 z >>= \z' -> mkPos z' InConcl ps
      (Just 0, [])     -> Nothing
      _                -> sibling 0 z >>= \z' -> mkPos z' InConcl (L : path)

applyVar :: Name -> Pos -> Maybe Pos
applyVar x (Pos z (Occ f path pol)) = case (f, pol) of
  (InConcl, N)  -> mkPos z (InPrem x) path
  (InPrem _, N) -> mkPos z InConcl path
  _             -> Nothing

-- Tuple node <t0, ..., tn-1>: children [t0 (0), ..., tn-1 (n-1)]
applyTensor :: Pos -> Maybe Pos
applyTensor (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> ownerOf y (childrenOf z) z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> up z >>= \z' -> mkPos z' (InPrem y) path
  -- component i of the conclusion  ->  conclusion of the i-th child
  (InConcl, N)  -> case path of
      C i : ps -> down i z >>= \z' -> mkPos z' InConcl ps
      _        -> Nothing
  -- leaving the k-th child  ->  component k of the conclusion
  (InConcl, P)  -> do
      k  <- childIndex z
      z' <- up z
      mkPos z' InConcl (C k : path)

-- Node let <x0, ..., xn-1> = M in N: children [tuple (0), body (1)]
applyDecomp :: [Name] -> Pos -> Maybe Pos
applyDecomp xs (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem v, N) -> ownerOf v [0, 1] z >>= \z' -> mkPos z' (InPrem v) path
  (InPrem v, P) -> case elemIndex v xs of
      -- x_i in the body  ->  component i of the tuple
      Just i  -> sibling 0 z >>= \z' -> mkPos z' InConcl (C i : path)
      Nothing -> up z >>= \z' -> mkPos z' (InPrem v) path
  (InConcl, N)  -> down 1 z >>= \z' -> mkPos z' InConcl path
  (InConcl, P)  -> case (childIndex z, path) of
      -- leaving the tuple: component i -> x_i in the body
      (Just 0, C i : ps) | i < length xs -> sibling 1 z >>= \z' -> mkPos z' (InPrem (xs !! i)) ps
      (Just 0, _)        -> Nothing
      -- leaving the body -> conclusion of the let
      _                  -> up z >>= \z' -> mkPos z' InConcl path

-- Indices of the children of a node
childrenOf :: Zipper -> [Int]
childrenOf z = [0 .. length (subForest (focus z)) - 1]

applyLambda :: Name -> Pos -> Maybe Pos
applyLambda x (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) | y /= x -> down 0 z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) | y /= x -> up z >>= \z' -> mkPos z' (InPrem y) path
  (InConcl, N) -> case path of
      L : ps -> down 0 z >>= \z' -> mkPos z' (InPrem x) ps
      R : ps -> down 0 z >>= \z' -> mkPos z' InConcl ps
      []     -> Nothing
  (InConcl, P)  -> up z >>= \z' -> mkPos z' InConcl (R : path)
  (InPrem _, P) -> up z >>= \z' -> mkPos z' InConcl (L : path)
  (InPrem _, N) -> Nothing

applyLet :: Name -> Pos -> Maybe Pos
applyLet x (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) | y /= x -> ownerOf y [0, 1] z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) | y /= x -> up z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem _, P) -> sibling 0 z >>= \z' -> mkPos z' InConcl path
  (InPrem _, N) -> Nothing
  (InConcl, P)  -> case childIndex z of
      Just 0 -> sibling 1 z >>= \z' -> mkPos z' (InPrem x) path
      _      -> up z >>= \z' -> mkPos z' InConcl path
  (InConcl, N)  -> down 1 z >>= \z' -> mkPos z' InConcl path

applyIf :: Pos -> Maybe Pos
applyIf (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem y, N) -> ownerOf y [0] z >>= \z' -> mkPos z' (InPrem y) path
  (InPrem y, P) -> case childIndex z of
      Just 0 -> up z >>= \z' -> mkPos z' (InPrem y) path
      _      -> Nothing
  (InConcl, _)  -> Nothing


applyRule :: Rule -> Pos -> Either String Pos
applyRule rule pos =
  let target = case rule of
        TLAMBDA x   -> applyLambda x pos
        TVAR x      -> applyVar x pos
        TAPP        -> applyApp pos
        TTENSOR     -> applyTensor pos
        TDECOMP xs  -> applyDecomp xs pos
        TLET x      -> applyLet x pos
        TGATE _ _   -> applyGate pos
        TIF         -> applyIf pos
  in case (rule, target) of
       (_, Just p)    -> Right p
       (_, Nothing)   -> Left ("Rule " ++ show rule ++ " not applicable to the position " ++ showPos pos)

----- Travel tokens -----

stepToken :: Token -> Config -> Either String (Token, Config)
stepToken tok cfg = do
  let pos@(Pos _ occ) = tokPos tok
  rule <- ruleAt pos
  case rule of
    -- The token is on the input of a gate axiom: emits the gate
    TGATE g [] | occPol occ == N -> do
      if isMultiGateAxiom (posNode pos)
        then Left ("A multi-input gate is fired by fireGate, not by stepToken: " ++ showPos pos)
        else Right ()
      pos' <- applyRule rule pos
      let n    = cfgNextLabel cfg + 1
          lOut = Lab n
      return ( tok { tokPos = pos', tokLabel = lOut }
             , cfg { cfgCircuit = modifyAt (tokAddr tok) (++ [Wire g (tokLabel tok) lOut]) (cfgCircuit cfg)
                   , cfgNextLabel = n } )
    _ -> do
      pos' <- applyRule rule pos
      return (tok { tokPos = pos' }, cfg)

-- Circuit rule for a gate with several inputs
fireGate :: String -> [Token] -> Config -> Either String ([Token], Config)
fireGate g toks cfg = do
  poss <- mapM (applyRule (TGATE g []) . tokPos) toks
  let n    = cfgNextLabel cfg
      outs = [ Lab (n + i) | i <- [1 .. length toks] ]
      gate = MultiGate g (map tokLabel toks) outs
      toks' = [ t { tokPos = p, tokLabel = l } | (t, p, l) <- zip3 toks poss outs ]
  case toks of
    []      -> Left ("fireGate: no tokens for gate " ++ g)
    (t : _) -> return ( toks'
                      , cfg { cfgCircuit = modifyAt (tokAddr t) (++ [gate]) (cfgCircuit cfg)
                            , cfgNextLabel = n + length toks } )

fireIf :: Zipper -> [Token] -> Token -> Config -> Either String ([Token], Config)
fireIf z toks bitTok cfg = do 
  (toksThen, circThen, cfgThen) <- runBranch z 1 toks cfg
  (toksElse, circElse, cfgElse) <- runBranch z 2 toks cfgThen
  let toksElse'                 = [(tokLabel tokElse, tokLabel tokThen) | tokElse <- toksElse,  
                                   tokThen <- toksThen, (posOcc . tokPos) tokElse == (posOcc . tokPos) tokThen]
  let applyRename               = map renamePerGate 
      renamePerGate g = case g of 
        Wire s l1 l2            -> Wire s (returnLabel l1) (returnLabel l2)
        MultiGate s ls1 ls2     -> MultiGate s (map returnLabel ls1) (map returnLabel ls2)
        Cond l c1 c2            -> Cond (returnLabel l) (map renamePerGate c1) (map renamePerGate c2)
      returnLabel l = fromMaybe l (lookup l toksElse') 
  if length toksElse' == length toksElse && length toksElse == length toksThen 
    then Right ()
    else Left ("Unmatched ocurrences" ++ show [toks' | toks' <- toksElse, tokThen <- toksThen, (posOcc . tokPos) toks' /= (posOcc . tokPos) tokThen])
  let note msg = maybe (Left msg) Right
      newGate    = Cond (tokLabel bitTok) circThen (applyRename circElse)
  listOfTokens <- note (show (pathOf z)) (mapM (reroot z) toksThen)
  return (listOfTokens, cfg {cfgCircuit = modifyAt (tokAddr bitTok) (++ [newGate]) (cfgCircuit cfg), cfgNextLabel = cfgNextLabel cfgElse})

fireAllIfs :: [Token] -> Config -> Either String ([Token], Config)
fireAllIfs toks cfg = case readyTokens waitingAtIf ifInputs toks of 
  Nothing -> Right (toks, cfg)
  Just (z, idxs) -> do
    (fired, cfg') <- fireIf z (map (toks !!) (init idxs)) (toks !! last idxs) cfg
    let toks' = [t | (i,t) <- zip [0..] toks, notElem i idxs]++fired
    fireAllIfs toks' cfg'


runBranch :: Zipper -> Int -> [Token] -> Config -> Either String ([Token], Circuit, Config)
runBranch z b toks cfg = do
  let note msg = maybe (Left msg) Right 
  zb    <- note "" (down b z)
  toks' <- note "" (mapM (reroot zb) toks)
  let stopCond p = posNode p == zb && occPol(posOcc p) == P
      cfgB       = emptyConfig {cfgTokens = toks', cfgNextLabel = cfgNextLabel cfg}
  (finalToks, cfgEnd) <- runLoop stopCond toks' cfgB
  circ                <- note "" (at emptyAddress (cfgCircuit cfgEnd))
  return (finalToks, circ, cfgEnd) 

 
reroot :: Zipper -> Token -> Maybe Token
reroot z' tok = do
  let Pos _ (Occ f path _) = tokPos tok
  p <- mkPos z' f path
  return tok { tokPos = p }


-- Positions that must all be occupied before a multi-input gate axiom fires
gateAxiomInputs :: Zipper -> [Pos]
gateAxiomInputs z = [ Pos z o | o <- axiomInputs z ]

-- Positions that must all be occupied before an if-then-else fires (synchronous rule):
ifInputs :: Zipper -> [Pos]
ifInputs z =
  [ Pos z o | o <- positionsOf z, occPol o == N, not (guardOwned o) ]
  ++ [ Pos z0 (Occ InConcl [] P) | Just z0 <- [down 0 z] ]
  where
    guardOwned (Occ (InPrem y) _ _) = isJust (ownerOf y [0] z)
    guardOwned _                    = False

readyTokens :: (Token -> Maybe Zipper) -> (Zipper -> [Pos]) -> [Token] -> Maybe (Zipper, [Int])
readyTokens inPlace required toks = listToMaybe
  [ (z, idxs)
  | (i, t) <- itoks
  , Just z <- [inPlace t]
  , let addr = tokAddr t
  , let occupants p = [ k | (k, t') <- itoks, tokPos t' == p, tokAddr t' == addr ]
  , let idxs = concatMap occupants (required z)
  , length idxs == length (required z)
  , i == head idxs ]
  where itoks = zip [0 ..] toks

-- Fires every ready gate, keeping the tokens in their places in the list
fireAllGates :: [Token] -> Config -> Either String ([Token], Config)
fireAllGates toks cfg = case readyTokens inMultiGate gateAxiomInputs toks of
  Nothing        -> Right (toks, cfg)
  Just (z, idxs) -> do
    g <- case termOf z of
           TGate g [] _ -> Right g
           _            -> Left ("fireAllGates: not a gate axiom at " ++ show (pathOf z))
    (fired, cfg') <- fireGate g (map (toks !!) idxs) cfg
    let toks' = foldl (\acc (i, t) -> take i acc ++ [t] ++ drop (i + 1) acc) toks (zip idxs fired)
    fireAllGates toks' cfg'

allNodes :: Zipper -> [Zipper]
allNodes z = z : concatMap allNodes [ zk | k <- [0 .. length (subForest (focus z)) - 1]
                                         , Just zk <- [down k z] ]

-- Initial positions: NDATA (negative positions of the conclusion of pi) and the outputs of the `new` axioms
findInitials :: Zipper -> [Pos]
findInitials root =
  let ndata     = [ Pos root o | o <- positionsOf root, occPol o == N ]
      isNew z   = case termOf z of TNew {} -> True; _ -> False
      newTokens = [ Pos z o | z <- allNodes root, isNew z, o <- positionsOf z, occPol o == P ]
  in ndata ++ newTokens

-- Creates the initial tokens, one label each
addTokens :: [Pos] -> Config -> (Config, TokenLabelList)
addTokens poss cfg =
  let start  = cfgNextLabel cfg
      toks   = zipWith (\p i -> Token p (Lab i) emptyAddress) poss [start + 1 ..]
      assoc  = [ (tokPos t, tokLabel t) | t <- toks ]
  in (cfg { cfgTokens = cfgTokens cfg ++ toks, cfgNextLabel = start + length poss }, assoc)

----- Identity Application -----

-- Identity on every initial wire 
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

----- Core Running Machine -----

runLoop :: (Pos -> Bool) -> [Token] -> Config -> Either String ([Token], Config)
runLoop stopCond = loop
  where
    waiting t = stopCond (tokPos t) || isJust (inMultiGate t) || isJust (waitingAtIf t)

    -- one step for one token
    go (done, c) t
      | waiting t = Right (done ++ [t], c)
      | otherwise = do
          (t', c') <- stepToken t c
          return (done ++ [t'], c')

    loop toks c
      | all (stopCond . tokPos) toks = Right (toks, c)
      | otherwise = do
          (toks1, c1) <- foldM go ([], c) toks
          (toks2, c2) <- fireAllGates toks1 c1
          (toks3, c3) <- fireAllIfs toks2 c2
          if map tokPos toks3 == map tokPos toks
            then Left ("Deadlock: tokens waiting at " ++ show
                       [ showPos (tokPos t) | t <- toks3, not (stopCond (tokPos t)) ])
            else loop toks3 c3

runMachine :: Config -> Either String (Circuit, TokenLabelList)
runMachine cfg0 = do
  let cfg1 = setupInitialTokens cfg0
  (finalToks, cfgEnd) <- runLoop stopCond (cfgTokens cfg1) cfg1 { cfgTokens = [] }
  circuit <- maybe (Left "Invalid empty address in the extended circuit")
                    Right (at emptyAddress (cfgCircuit cfgEnd))
  return (circuit, [ (tokPos t, tokLabel t) | t <- finalToks ])


startMachine :: TypeDerivation -> Either String (Circuit, TokenLabelList, TokenLabelList)
startMachine derivation = do
  let root             = fromTree derivation
      (cfg, assocList) = addTokens (findInitials root) emptyConfig
  (final, finalList) <- runMachine cfg
  return (final, assocList, finalList)


----- Pretty -----

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