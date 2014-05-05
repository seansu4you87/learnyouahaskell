-- Stateful computation is a function that takes some state and returns a value
-- with some state

-- s -> (a,s)

type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((),newStack1) = push 3 stack
  (a, newStack2) = pop newStack1
  in pop newStack2

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  State h >>= f = State $ \s -> let (a,newState) = h s
                                    (State g) = f a
                                in g newState

  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) :: State (s -> (a,s)) -> (a -> State (s -> (b,s))) -> State (s -> (b,s))
  (a,newState) = s -> (a,s)) s
  State g = (a -> State (s -> (b,s))) a = State (s -> (b,s))
  g newState = (s -> (b,s)) newState = (b,newState)
  type (b,s)
  put in lambda \s -> (b,s)
  apply State = State $ \s -> (b,s) :: State s b


pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

stackManip2 :: State Stack Int
stackManip2 = do
  a <- pop
  if a == 5
    then push 5
    else 
      push 3
      push 8
