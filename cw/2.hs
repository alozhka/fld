import Modulemy

main :: IO ()
main = do
  print (ex1 [1, 2, 5, 3, 6, 5, 8, 6, 10] 6)
  print (ex1 [1, 2, 4, 1, 3, 1, 1, 6, 10] 6)

  putStrLn ""
  print (ex2 3)
  -- print (ex2 0)

  putStrLn ""
  print (ex3 [1,2,3,4,5,6,7,8] [1,2,3,4,5,6,7,8])
  print (ex3 [1,2,3,4,5] [2,4,7,6,5])
  print (ex3 [1,2,3,4,5,6,7,8] [2,3,4,5,6,7,8,9])

  putStrLn ""
  ex4 "input.csv" "out.txt"
  contents <- readFile "out.txt"
  putStr contents