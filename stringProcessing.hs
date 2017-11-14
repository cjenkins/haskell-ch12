module StringProcessing where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe s = init $ foldr restringify "" $ map notThe (words s) where
  restringify Nothing b = "a " ++ b
  restringify (Just a) b = a ++ " " ++ b

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel s@(' ':ss) = countTheBeforeVowel ss
countTheBeforeVowel s = shouldCount (words s) + countTheBeforeVowel (dropWhile (\x -> x /= ' ') s) where
  shouldCount xs@(x:y:zs) = if x == "the" && elem (y !! 0) "aeiou" then 1 else 0
  shouldCount xs@(x:[]) = 0

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

getVowelsOfString :: String -> String
getVowelsOfString = filter isVowel

countVowels :: String -> Int
countVowels = length . getVowelsOfString

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if (length s - countVowels s) < countVowels s then Nothing else Just (Word' s)
