import Data.IORef

magic :: IORef (Maybe Int) -> IO ()
magic ref = do
  value <- readIORef ref

  case value of
   Just _ -> return ()
   Nothing -> writeIORef ref (Just 42)


main :: IO ()
main = do
  ref <- newIORef Nothing
  magic ref
  val <- readIORef ref

  case val of
    Just x -> print x
    Nothing -> print "Empty"
  
