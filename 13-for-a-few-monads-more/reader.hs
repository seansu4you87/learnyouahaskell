instance Monad ((->) r) where
  return x = \_ -> x
  h >>= f = \w -> f (h w) w

  (>>=) :: m a -> (a -> m b) -> m b
  where m = r -> a
  (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  (a -> r -> b) ((r -> a) r) r
  (a -> r -> b) a r
  (r -> b) r
  b

  r -> b



