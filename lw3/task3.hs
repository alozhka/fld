import System.IO

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] [] = []
myunion [] (y:ys)
    | y `elem` ys = myunion [] ys
    | otherwise = y : myunion [] ys
myunion (x:xs) ys
    | x `elem` ys = myunion xs ys
    | x `elem` xs = myunion xs ys
    | otherwise = x : myunion xs ys

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let l1 = [-9, 1, 1, 2, 3] :: [Int]
    let l2 = [1, 2, 0, 22, -5] :: [Int]
    print (myunion l1 l2)