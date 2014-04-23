import qualified Data.Foldable as F

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f Node a left right = F.foldMap f l `mappend`
                                f x           `mappend`
                                F.foldMap f r
