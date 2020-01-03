{-# LANGUAGE ScopedTypeVariables #-}

module Replace where

import Universum

-- Given a list U and values Y and Z, return a copy of U with the LAST INSTANCE of Y replaced by Z.
-- If Y is not a member of U, return U.
-- Example: U = (1 2 3 2 1), Y = 2, Z = 5, result = (1 2 3 5 1)

replaceLastWith :: Eq a => a -> a -> [a] -> [a]
replaceLastWith y z u = reverse . replaceFirstWith y z $ reverse u

replaceFirstWith :: Eq a => a -> a -> [a] -> [a]
replaceFirstWith _ _ [] = []
replaceFirstWith y z (x : xs) | x == y = z : xs
replaceFirstWith y z (x : xs) = x : replaceFirstWith y z xs

replaceLastWithAcc :: Eq a => a -> a -> [a] -> [a]
replaceLastWithAcc y z u = reverse . replaceFirstWithAcc y z $ reverse u

replaceFirstWithAcc :: Eq a => a -> a -> [a] -> [a]
replaceFirstWithAcc y z u = replaceAcc [] False u
  where
    replaceAcc acc _ [] = reverse acc
    replaceAcc acc True xs = reverse acc ++ xs
    replaceAcc acc False (x : xs) | x == y = replaceAcc (z : acc) True xs
    replaceAcc acc False (x : xs) = replaceAcc (x : acc) False xs

fun :: [a] -> Int
fun [] = 7
fun (x:xs) = fun xs

replaceLastWith' :: forall a. Eq a => a -> a -> [a] -> [a]
replaceLastWith' y z u = fst $ replaceIn u
  where
    replaceIn :: [a] -> ([a], Bool)
    replaceIn [] = ([], False)
    replaceIn (x : xs) | x == y =
      case replaceIn xs of
        (xs', False) -> (z : xs', True)
        (xs', True ) -> (x : xs', True)
    replaceIn (x : xs) =
      let (rxs, replaced) = replaceIn xs in
        (x : rxs, replaced)

replaceTest :: IO ()
replaceTest = do
  let
    y = 2
    z = 5
    u = [1, 2, 3, 2, 1]
  print $ replaceFirstWith y z u
  print $ replaceFirstWithAcc y z u
  print $ replaceLastWithAcc y z u
  print $ replaceLastWith y z u
  print $ replaceLastWith' y z u
