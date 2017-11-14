module BinaryTree where

import Data.Maybe

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

fst3 (x, _, _) = x
snd3 (_, y, _) = y
thd3 (_, _, z) = z

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a
  | isNothing (f a) = Leaf
  | otherwise = Node
                (unfold f (fst3 (fromJust (f a))))
                (snd3 (fromJust (f a)))
                (unfold f (thd3 (fromJust (f a))))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold foldFn n where
  foldFn 0 = Nothing
  foldFn n = Just(n - 1, n, n - 1)
