data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

-- making trafficlight a type instance of the eq typeclass.  since we
-- recursively defined (==) and (/=), we only needed to fill out one of the
-- functions (we choose (==)).  this is called the minimal complete definition
-- for the typeclass, the minimum of functions that we have to implement so
-- that our type can behave like the class advertises.

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Green = "Green Light"
  show Yellow = "Yellow Light"

instance (Eq m) => Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
