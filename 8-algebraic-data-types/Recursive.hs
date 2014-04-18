-- normal syntax
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- record syntax
-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

-- `Cons` is another word for `:`
-- `:` is a constructor that takes a value and another list, and returns a list

-- explicitly defining infix functions:
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
