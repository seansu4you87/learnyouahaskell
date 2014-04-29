-- isBigGang :: Int -> (Bool,String)
-- isBigGang x = (x > 9,"Compared to gang size 9.")

-- applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
-- applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
-- applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- import Data.Monoid

-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where 
--   return x = Writer (x, mempty)
--   (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)
