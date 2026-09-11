pyramidular :: Int -> [Int]
pyramidular n = [x * (x + 1) * (x + 2) `div` 6 | x <- [1..n]]

main :: IO ()

main = do
    print (pyramidular 50)