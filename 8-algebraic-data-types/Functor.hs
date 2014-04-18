import Tree

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

-- Either a b = Left a | Right b
-- fmap  (b -> c) -> Either a b -> Either a c
-- f (Right x) = Right (f x)
-- f (Left x) = Left x
instance Functor Either a where
  fmap f (Right x) = Right (f x)
  fmap f (Left x)  = Left x

-- Map k v = [(k,v)]
-- fmap (a -> b) -> Map k a -> Map k b
-- f (Map k v) = Map k (f v)

instance Functor Map k where
  fmap f (Map k v) = Map k (f v)
