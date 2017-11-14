module EitherLibrary where

lefts' :: [Either a b] -> [a]
lefts' es = foldr extractLeft [] es where
  extractLeft (Left a) as = (a:as)

rights' :: [Either a b] -> [b]
rights' es = foldr extractRight [] es where
  extractRight (Right b) bs = (b:bs)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f (Left a)  = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' ac _ (Left a) = ac a
either' _ bc (Right b) = bc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f eab = either' (\x -> Nothing) (\x -> Just (f x)) eab
