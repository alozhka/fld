import System.IO

mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst (x:xs) ys
    | x `elem` ys = mysubst xs ys
    | otherwise = x : mysubst xs ys

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let l1 = [-9, -9, 1, 2, 3] :: [Int]
    let l2 = [1, 2, 0, 22, -5] :: [Int]
    print (mysubst l1 l2)