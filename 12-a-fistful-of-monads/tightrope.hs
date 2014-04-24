type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n,right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
  | abs ((right + n) - left) < 4 = Just (left,right + n)
  | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

Just 9 >>= (\x -> Just (x > 8))

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)


return (0,0) >>= landLeft 2 >>= landRight 1

routine :: Maybe Pool
routine = do
  start <- return (0,0)
  step1 <- landLeft 2 start
  landRight 1 step1

  -- Equivalent expressions
  Nothing
  _ <- Nothing
