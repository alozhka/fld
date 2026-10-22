import System.IO
import Text.Read (readMaybe)

readInt :: String -> IO Int
readInt prompt = do
  putStr prompt
  str <- getLine
  case readMaybe str of
    Just x -> return x
    Nothing -> putStrLn "Error: enter an integer" >> readInt prompt

multiples :: Int -> Int -> Int -> [Int]
multiples start n k = take n (filter (\x -> x `mod` k == 0) [start ..])

printMultiples :: Int -> Int -> Int -> IO ()
printMultiples start n k =
  if k <= 0 || n < 0
    then putStrLn "Error: multiplicity must be positive and amount must be non-negative"
    else do
      if start `mod` k /= 0
        then putStrLn ("Note: " ++ show start ++ " is not a multiple of " ++ show k)
        else return ()
      print (multiples start n k)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering

  start <- readInt "Enter initial value: "
  n <- readInt "Enter elements amount: "
  k <- readInt "Enter multiplicity: "

  printMultiples start n k