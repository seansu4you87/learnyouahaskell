newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

newtype Pair b a = Pair { getPair :: (a,b) }
instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x,y)
