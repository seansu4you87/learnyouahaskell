instance (Error e) => Monad (Either e) where 
  return x = Right x
  Right x >>= f = f x
  Left e >>= f = Left e
  fail msg = Left (strMsg msg)
