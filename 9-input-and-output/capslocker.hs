import Control.Monad
import Data.Char

main = forever $ do
  content <- getContents
  putStrLn $ map toUpper content
