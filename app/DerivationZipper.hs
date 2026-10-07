module DerivationZipper where

import CreateDerivation (Tree(..), TypeDerivation, Concl, Prem)
import TypeTree (TypedTerm)

data Crumb = Crumb
  { parentLabel :: Concl
  , leftSibs    :: [TypeDerivation]   
  , rightSibs   :: [TypeDerivation]
  }

data Zipper = Zipper
  { focus  :: TypeDerivation
  , crumbs :: [Crumb]                
  }

fromTree :: TypeDerivation -> Zipper
fromTree t = Zipper t []

up :: Zipper -> Maybe Zipper            
up (Zipper _ []) = Nothing
up (Zipper t (Crumb lbl ls rs : cs)) = Just (Zipper (Node lbl (reverse ls ++ t : rs)) cs)

down :: Int -> Zipper -> Maybe Zipper   -- to the i-th premise
down i (Zipper (Node lbl kids) cs) = case splitAt i kids of
  (ls, t : rs) | i >= 0 -> Just (Zipper t (Crumb lbl (reverse ls) rs : cs))
  _                     -> Nothing

sibling :: Int -> Zipper -> Maybe Zipper
sibling i z = up z >>= down i

childIndex :: Zipper -> Maybe Int       
childIndex (Zipper _ [])      = Nothing
childIndex (Zipper _ (c : _)) = Just (length (leftSibs c))

isRoot :: Zipper -> Bool                
isRoot = null . crumbs

pathOf :: Zipper -> [Int]               
pathOf = reverse . map (length . leftSibs) . crumbs

-- Reading the focused judgement.
judgement :: Zipper -> Concl
judgement = rootLabel . focus

premOf :: Zipper -> Prem
premOf z = let (p, _, _) = judgement z in p

termOf :: Zipper -> TypedTerm
termOf z = let (_, t, _) = judgement z in t

instance Eq Zipper where a == b = pathOf a == pathOf b
instance Show Zipper where show = show . pathOf