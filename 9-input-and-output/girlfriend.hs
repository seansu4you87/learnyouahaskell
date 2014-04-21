import System.IO

-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

main = do
  withFile "girlfriend.txt" ReadMode (\handle -> do
    contexts <- hGetContents handle
    putStr contexts)


withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path, mode, f = do
  handle = openFile path mode
  result <- f handle
  hClose handle
  return result
