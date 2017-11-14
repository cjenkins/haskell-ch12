module ItsOnlyNatural where

data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = Just (Succ (safeIntegerToNat (i - 1))) where
      safeIntegerToNat i = if i == 0 then Zero else Succ (safeIntegerToNat (i - 1))
