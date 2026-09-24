import System.IO

secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist ([]:xs) = secondlastlist xs
secondlastlist (xs:xss) = last xs : secondlastlist xss

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let sublists = [[1, 2, 3], [0, 22, -5], [111, 555, 777]]
    print (secondlastlist sublists)