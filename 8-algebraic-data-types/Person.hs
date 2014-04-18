data Person = Person String String Int Float String String deriving (Show)

data Person = Person { firstName :: String
                     , lastname :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
