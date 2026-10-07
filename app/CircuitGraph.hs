module CircuitGraph where

import Control.Monad (foldM)
import qualified Data.Map as Map
import TypeTree (Type(..), Name, TypedTerm(..), TypedValue(..))
import CreateDerivation (Concl, TypeDerivation, Tree(..))
import DerivationZipper
import Data.Maybe (listToMaybe, isJust)


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

data Gate
  = Wire String Label Label           -- I, H, X, Y, Z, T, M   (labelIn, labelOut)
  | CNOT Label Label Label Label      -- inControl, outControl, inTarget, outTarget
  deriving (Show, Eq)

type Circuit = [Gate]

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
    walk pol (TPair a _)  (L : ps) = walk pol a ps
    walk pol (TPair _ b)  (R : ps) = walk pol b ps
    walk _   _            _        = Nothing

mkPos :: Zipper -> Formula -> [Position] -> Maybe Pos
mkPos z f p = Pos z . Occ f p <$> polarityAt (judgement z) f p

positionsOf :: Zipper -> [Occ]
positionsOf z =
  let (prem, _, ty) = judgement z
      inType f start t = map (uncurry (Occ f)) (walk start [] t)
      walk pol acc t | isBase t = [(reverse acc, pol)]
      walk pol acc (TFun a b)   = walk (flipPol pol) (L : acc) a ++ walk pol (R : acc) b
      walk pol acc (TPair a b)  = walk pol (L : acc) a ++ walk pol (R : acc) b
      walk _   _   _            = []
  in concat [ inType (InPrem x) N t | (x, t) <- Map.toList prem ] ++ inType InConcl P ty

stopCond :: Pos -> Bool
stopCond (Pos z occ) = isRoot z && occPol occ == P

isGateAxiom :: Zipper -> Bool
isGateAxiom z = case termOf z of
  TGate _ [] _ -> True
  _            -> False

-- if the token is waiting on an input of a CNOT axiom: path [L, side], with L = input of the arrow and side = control (L) / target (R)
inCNOT :: Token -> Maybe (Zipper, Position)
inCNOT tok = case tokPos tok of
  Pos z (Occ InConcl [L, side] N) | isCNOT z -> Just (z, side)
  _                                          -> Nothing
  where
    isCNOT z = case termOf z of
      TGate "CNOT" [] _ -> True
      _                 -> False

----- Application of rules, following the paper -----

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

applyDecomp :: Name -> Name -> Pos -> Maybe Pos
applyDecomp x y (Pos z (Occ f path pol)) = case (f, pol) of
  (InPrem v, N) -> ownerOf v [0, 1] z >>= \z' -> mkPos z' (InPrem v) path
  (InPrem v, P)
      | v == x    -> sibling 0 z >>= \z' -> mkPos z' InConcl (L : path)
      | v == y    -> sibling 0 z >>= \z' -> mkPos z' InConcl (R : path)
      | otherwise -> up z >>= \z' -> mkPos z' (InPrem v) path
  (InConcl, N)  -> down 1 z >>= \z' -> mkPos z' InConcl path
  (InConcl, P)  -> case (childIndex z, path) of
      (Just 0, L : ps) -> sibling 1 z >>= \z' -> mkPos z' (InPrem x) ps
      (Just 0, R : ps) -> sibling 1 z >>= \z' -> mkPos z' (InPrem y) ps
      (Just 0, [])     -> Nothing
      _                -> up z >>= \z' -> mkPos z' InConcl path

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

----- Travel tokens -----

stepToken :: Token -> Config -> Either String (Token, Config)
stepToken tok cfg = do
  let pos@(Pos _ occ) = tokPos tok
  rule <- ruleAt pos
  case rule of
    -- The token is on the input of a gate axiom: emits the gate
    TGATE g [] | occPol occ == N -> do
      case g of
        "CNOT" -> Left ("A CNOT is fired by fireCNOT, not by stepToken: " ++ showPos pos)
        _      -> Right ()
      pos' <- applyRule rule pos
      let n    = cfgNextLabel cfg + 1
          lOut = Lab n
      return ( tok { tokPos = pos', tokLabel = lOut }
             , cfg { cfgCircuit = modifyAt (tokAddr tok) (++ [Wire g (tokLabel tok) lOut]) (cfgCircuit cfg)
                   , cfgNextLabel = n } )
    _ -> do
      pos' <- applyRule rule pos
      return (tok { tokPos = pos' }, cfg)

-- Circuit rule for CNOT
fireCNOT :: Token -> Token -> Config -> Either String (Token, Token, Config)
fireCNOT ctl tgt cfg = do
  ctlPos <- applyRule (TGATE "CNOT" []) (tokPos ctl)
  tgtPos <- applyRule (TGATE "CNOT" []) (tokPos tgt)
  let n    = cfgNextLabel cfg
      out1 = Lab (n + 1)
      out2 = Lab (n + 2)
      gate = CNOT (tokLabel ctl) out1 (tokLabel tgt) out2
  return ( ctl { tokPos = ctlPos, tokLabel = out1 }
         , tgt { tokPos = tgtPos, tokLabel = out2 }
         , cfg { cfgCircuit = modifyAt (tokAddr ctl) (++ [gate]) (cfgCircuit cfg)
               , cfgNextLabel = n + 2 } )

-- Indices (control, target) of the first CNOT with both inputs occupied
readyCNOT :: [Token] -> Maybe (Int, Int)
readyCNOT toks = listToMaybe
  [ (i, j)
  | (i, ctl) <- itoks, Just (z1, L) <- [inCNOT ctl]
  , (j, tgt) <- itoks, Just (z2, R) <- [inCNOT tgt]
  , z1 == z2, tokAddr ctl == tokAddr tgt ]
  where itoks = zip [0 ..] toks


-- Fires every ready CNOT, keeping the tokens in their places in the list
fireAllCNOT :: [Token] -> Config -> Either String ([Token], Config)
fireAllCNOT toks cfg = case readyCNOT toks of
  Nothing     -> Right (toks, cfg)
  Just (i, j) -> do
    (ctl', tgt', cfg') <- fireCNOT (toks !! i) (toks !! j) cfg
    let toks'  = take i toks  ++ [ctl'] ++ drop (i + 1) toks
        toks'' = take j toks' ++ [tgt'] ++ drop (j + 1) toks'
    fireAllCNOT toks'' cfg'


allNodes :: Zipper -> [Zipper]
allNodes z = z : concatMap allNodes [ zk | k <- [0 .. length (subForest (focus z)) - 1]
                                         , Just zk <- [down k z] ]

-- Initial positions: NDATA (negative positions of the conclusion of pi) and the outputs of the `new` axioms
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

runMachine :: Config -> Either String (Circuit, TokenLabelList)
runMachine cfg0 = do
  let cfg1 = setupInitialTokens cfg0

      -- one step for one token finished tokens and tokens waiting at a CNOT are left alone
      go (done, c) t
        | stopCond (tokPos t) || isJust (inCNOT t) = Right (done ++ [t], c)
        | otherwise = do
            (t', c') <- stepToken t c
            return (done ++ [t'], c')

      -- every free token moves one step, then the ready CNOTs fire and repeats until all tokens have finished
      loop toks c
        | all (stopCond . tokPos) toks = Right (toks, c)
        | otherwise = do
            (toks1, c1) <- foldM go ([], c) toks
            (toks2, c2) <- fireAllCNOT toks1 c1
            if map tokPos toks2 == map tokPos toks
              then Left ("Deadlock: tokens waiting at " ++ show
                         [ showPos (tokPos t) | t <- toks2, not (stopCond (tokPos t)) ])
              else loop toks2 c2

  (finalToks, cfgEnd) <- loop (cfgTokens cfg1) cfg1 { cfgTokens = [] }
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