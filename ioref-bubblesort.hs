import Data.IORef
import Data.Foldable
import Control.Monad

bubbleSort :: [Int] -> IO [Int]
bubbleSort input = do
  let n = length input

  xs <- mapM newIORef input
  forM_ [0..n-1] $ \i -> do
    forM_ [0..n-2] $ \j -> do
      let ix = xs !! j
      let iy = xs !! (j + 1)

      x <- readIORef ix
      y <- readIORef iy

      when (x > y) $ do
        writeIORef ix y
        writeIORef iy x

  mapM readIORef xs


main :: IO ()
main = do
  let xs = [9, 2, 15, 7]
  sorted_xs <- bubbleSort xs
  print sorted_xs
    
    
