import System.IO
import Text.Read (readMaybe)

readInt :: String -> IO Int
readInt prompt = do
    putStr prompt
    line <- getLine
    case readMaybe line of
        Just v  -> return v
        Nothing -> do
            putStrLn "Ошибка: введите целое число"
            readInt prompt

listnums :: Int -> [Int]
listnums n
    | n < 1 = []
    | otherwise = n : listnums (n - 1)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    n <- readInt "Число: "
    print (listnums n)