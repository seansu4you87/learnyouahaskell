typeclass Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor IO where
  fmap f action = do
    a <- action
    return (f a)

instance Functor ((->) r) where
  fmap :: (a -> b) -> (r -> a) -> (r -> b)
  -- fmap f g = (\x -> f (g x))
  fmap = (.)

-- fmap g $ fmap f x == fmap (g . f) x

data CMaybe a = CNothing | CJust Int a deriving (Show)

-- NOT a functor!  Doesn't obey the functor law:
-- Functor f: fmap id f == id f
instance Functor CMaybe where
  fmap f Nothing = Nothing
  fmap f Just counter a = Just (counter+1) (f a)
