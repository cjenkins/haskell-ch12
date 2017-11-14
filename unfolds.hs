module Unfolds where

import Data.Maybe

myIterate :: (a -> a) -> a -> [a]
myIterate f a = (f a: myIterate f (f a))

myUnfoldr :: (b -> Maybe(a, b)) -> b -> [a]
myUnfoldr f b
  | isNothing (f b) = []
  | otherwise = ((fst (fromJust (f b))): myUnfoldr f (snd (fromJust (f b))))

myIterate' :: (a -> a) -> a -> [a]
myIterate' f a = myUnfoldr (\b -> Just(b, f b)) a
