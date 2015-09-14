import Prelude

data Tree a = Node (Tree a) a (Tree a) 
            | Empty
              deriving Show

areTreesEqual :: Tree Int -> Tree Int -> Bool
areTreesEqual Empty Empty = True
areTreesEqual Empty _ = False
areTreesEqual _ Empty = False
areTreesEqual (Node leftTree1 a rightTree1) (Node leftTree2 b rightTree2)  =
  if a == b then
    (areTreesEqual leftTree1 leftTree2) && (areTreesEqual rightTree1 rightTree2)
  else
    False



main :: IO ()
main = do
  --
  let a = Empty
      b = Empty
    in putStrLn $ show (areTreesEqual a b)

  let a = Empty
      b = Node Empty 3 (Node Empty 2 Empty)
    in putStrLn $ show (areTreesEqual a b)

  let a = Node Empty 3 (Node Empty 2 Empty)
      b = Empty
    in putStrLn $ show (areTreesEqual a b)
  
  let a = Node Empty 3 (Node Empty 2 Empty)
      b = Node Empty 3 (Node Empty 2 Empty)
    in putStrLn $ show (areTreesEqual a b)

  let a = Node Empty 3 (Node Empty 2 Empty)
      b = Node (Node Empty 3 (Node Empty 4 Empty)) 3 (Node Empty 2 Empty)
    in putStrLn $ show (areTreesEqual a b)
