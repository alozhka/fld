import System.IO

nposlist :: [[a]] -> Int -> [a]
nposlist l n = map (\subl -> subl !! n) (filter ((>n) . length) l)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    let sublists = [[1, 2, 3, 4], [0, 22, -5], [111, 555, 777, 999]]
    let n = 3
    print (nposlist sublists n)