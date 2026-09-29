module DerivationZipper where

import CreateDerivation (Tree(..), TypeDerivation, Concl, Prem)
import TypeTree (TypedTerm, Type)

-- One layer of context: the parent's judgement and the siblings of the focus.
data Crumb = Crumb
  { parentLabel :: Concl
  , leftSibs    :: [TypeDerivation]   -- reversed: nearest sibling first
  , rightSibs   :: [TypeDerivation]
  }

data Zipper = Zipper
  { focus  :: TypeDerivation
  , crumbs :: [Crumb]                 -- innermost first; [] at the root
  }

fromTree :: TypeDerivation -> Zipper
fromTree t = Zipper t []

toTree :: Zipper -> TypeDerivation      -- recover π from any token
toTree z = case up z of
  Nothing -> focus z
  Just z' -> toTree z'

-- Navigation (each O(1) apart from `down`, which is O(i)).
up :: Zipper -> Maybe Zipper            -- to the conclusion of the rule below
up (Zipper _ []) = Nothing
up (Zipper t (Crumb lbl ls rs : cs)) = Just (Zipper (Node lbl (reverse ls ++ t : rs)) cs)

down :: Int -> Zipper -> Maybe Zipper   -- to the i-th premise
down i (Zipper (Node lbl kids) cs) = case splitAt i kids of
  (ls, t : rs) | i >= 0 -> Just (Zipper t (Crumb lbl (reverse ls) rs : cs))
  _                     -> Nothing

sibling :: Int -> Zipper -> Maybe Zipper
sibling i z = up z >>= down i

childIndex :: Zipper -> Maybe Int       -- replaces `last pi`
childIndex (Zipper _ [])      = Nothing
childIndex (Zipper _ (c : _)) = Just (length (leftSibs c))

isRoot :: Zipper -> Bool                -- replaces `null pi`
isRoot = null . crumbs

pathOf :: Zipper -> [Int]               -- the old [PosInPi], for Eq/Show only
pathOf = reverse . map (length . leftSibs) . crumbs

-- Reading the focused judgement.
judgement :: Zipper -> Concl
judgement = rootLabel . focus

premOf :: Zipper -> Prem
premOf z = let (p, _, _) = judgement z in p

termOf :: Zipper -> TypedTerm
termOf z = let (_, t, _) = judgement z in t

typeOf :: Zipper -> Type
typeOf z = let (_, _, t) = judgement z in t

instance Eq Zipper where a == b = pathOf a == pathOf b
instance Show Zipper where show = show . pathOf