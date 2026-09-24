import System.IO

mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] ys = []
mysubst (x:xss) ys
    | x `elem` ys = mysubst xss ys
    | x `elem` xss = mysubst xss ys
    | otherwise = x : mysubst xss ys

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let l1 = [-9, -9, 1, 2, 3]
    let l2 = [1, 2, 0, 22, -5]
    print (mysubst l1 l2)