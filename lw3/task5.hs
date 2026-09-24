import System.IO

nposlist :: [[a]] -> Int -> [a]
nposlist l n
    | n < 1 = []
    | otherwise = map (!! (n - 1)) (filter hasEnoughLength l)
    where
        hasEnoughLength = not . null . drop (n - 1)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let sublists :: [[Int]] = [[1, 2, 3, 4], [0, 22, -5], [111, 555, 777, 999], [1,2..]] 
    let n = 3
    print (nposlist sublists n)