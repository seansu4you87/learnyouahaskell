import Data.List
import Data.Char

-- In gchi:
-- ghci> :m + Data.List Data.Map Data.Set
--
-- Only import select functions:
-- import Data.List (nub, sort)
--
-- Import a module except for select functions:
-- import Data.List hiding (nub)
--
-- Qualified Imports:
-- import qualified Data.Map
--
-- Now we must refer to Data.Map's filter as Data.Map.filter.
-- To use an alias:
-- import qualified Data.Map as M
--
-- Now we can refer to Data.Map's filter as M.filter
--
-- http://www.haskell.org/ghc/docs/latest/html/libraries/

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in  map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

findKeyRecursive :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyRecursive key [] = Nothing
findKeyRecursive key ((k,v):xs) = if key == k
                            then Just v
                            else Maybe findKeyRecursive key xs

findKeyFold :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyFold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
