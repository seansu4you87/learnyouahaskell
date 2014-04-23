class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

-- Monoid Laws
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

instance Monoid [a] where
  mempty = []
  mappend = (++)

newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty :: Product 1
  Product x `mappend` Product y = Product (x * y)

instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT


-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = length x `compare` length y
--                         b = x `compare` y
--                     in if a == EQ then b else a

lengthCompare :: String -> String -> STring
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
  where vowels = length . filter (`elem` "aeiow")


instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing

  m `mappend` Nothing = m
  Nothing `mappend` m = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First { getFirst :: Maybe a }
  deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x
