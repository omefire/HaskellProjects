import Data.IORef

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  modifyIORef ref (+1)
  val <- readIORef ref
  print val
  
