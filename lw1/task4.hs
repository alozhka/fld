triangular :: Int -> [Int]
triangular n = [(x * (x + 1)) `div` 2 | x <- [1..n]]

main :: IO ()

main = do
    print (triangular 50)