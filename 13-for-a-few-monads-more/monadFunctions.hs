liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

liftM f m = do
  x <- m
  return (f x)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return (f x)

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

join :: (Monad m) => m (m a) -> m a
join mm = do
  m <- mm
  m

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
