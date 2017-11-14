module MaybeLibrary where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just a) = False
isNothing Nothing = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = f a
mayybee b f Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just a2) = a2
fromMaybe a Nothing = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe l = Just (head l)

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes ms = concat $ map maybeToList $ filter isJust ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe ms = if length (catMaybes ms) == 0
               then Nothing
               else Just (catMaybes ms)
