import System.IO
import Text.Read (readMaybe)

readInt :: String -> IO Int
readInt prompt = do
  putStr prompt
  str <- getLine
  case readMaybe str of
    Just x -> return x
    Nothing -> putStrLn "Error: enter an integer" >> readInt prompt

firstMultiple :: Int -> Int -> Int
firstMultiple start k = ((start + k - 1) `div` k) * k

multiples :: Int -> Int -> Int -> [Int]
multiples start n k = take n [firstMultiple start k, firstMultiple start k + k ..]

printMultiples :: Int -> Int -> Int -> IO ()
printMultiples start n k =
  if k <= 0 || n < 0
    then putStrLn "Error: multiplicity must be positive and amount must be non-negative"
    else do
      let first = firstMultiple start k
          if first /= start
            then
              putStrLn "Error: no multiples found"
            else
              return ()
      print (multiples start n k)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering

  start <- readInt "Print initial value: "
  n <- readInt "Print elements amount: "
  k <- readInt "Print multiplicity: "

  printMultiples start n k