module TreeZipper where

import CreateDerivation (Tree(..), Concl)

data Context a = Context 
  { parentLabel :: a
  , leftSubTrees  :: [Tree a]  
  , rightSubTrees :: [Tree a]
  } deriving (Show, Eq)

type PathHistory a = [Context a]
type Zipper a = (Tree a, PathHistory a)

toZipper :: Tree a -> Zipper a
toZipper tree = (tree, [])

extractZipper :: Maybe (Zipper a) -> Zipper a
extractZipper (Just z) = z

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing 
goUp (focus, Context p l r : hs) =
    let recombinedForest = reverse l ++ [focus] ++ r
        parentNode = Node p recombinedForest
    in Just (parentNode, hs)

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Node _ [], _) = Nothing 
goDown (Node label (child:r), hs) =
    Just (child, Context label [] r : hs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (_, []) = Nothing 
goRight (_, Context _ _ [] : _) = Nothing 
goRight (focus, Context p l (r:rs) : hs) =
    Just (r, Context p (focus : l) rs : hs)

myZipper = (Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []},Node {rootLabel = 5, subForest = []}]},Node {rootLabel = 3, subForest = [Node {rootLabel = 6, subForest = []},Node {rootLabel = 7, subForest = []}]}]},[])
