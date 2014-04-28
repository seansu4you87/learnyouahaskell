applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >> \_ -> y

  fail :: String -> m a
  fail msg = error msg

instance Monad Maybe where
  return x = Just x

  Nothing >>= f = Nothing
  Just x >>= f = f x

  fail _ = Nothing

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _ = []


-- Monads that also act as Monoids
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a > m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- Monad Laws:
-- Left:          return x >>= f == f x
-- Right:         m >>= return == m
-- Associativity: f <=< (g <=< h) == (f <=< g) <=< h

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)
