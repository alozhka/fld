import System.IO

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] ys = ys
myunion (x:xss) ys
    | x `elem` ys = myunion xss ys
    | x `elem` xss = myunion xss ys
    | otherwise = x : myunion xss ys

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let l1 = [-9, 1, 1, 2, 3]
    let l2 = [1, 2, 0, 22, -5]
    print (myunion l1 l2)