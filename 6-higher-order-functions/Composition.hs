(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- addSquareSum :: Integer
-- addSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Can be written in free point style:
-- addSquareSum :: Integer
-- addSquareSum sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Or for more readability:
addSquareSum :: Integer
addSquareSum = 
  let addSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) addSquares
  in  sum belowLimit
