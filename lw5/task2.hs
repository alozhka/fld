import System.IO
import System.Environment (getArgs)
import Data.Char (isPunctuation)

printUsage :: IO ()
printUsage = 
    putStrLn "Usage: task2 <inputFilename> <outputFilename>"

replace :: Char -> String -> String
replace ch = map (\x -> if isPunctuation x then ch else x)

replacePunctuations :: Char -> String -> String -> IO ()
replacePunctuations ch inputFilename outputFilename = do
    text <- readFile inputFilename
    writeFile outputFilename (replace ch text)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    args <- getArgs
    case args of
        [input, output] -> do
            putStr "A character to change: "
            line <- getLine
            case line of
                [ch] -> replacePunctuations ch input output
                _ -> putStrLn "Error: a character was expected"
        _ -> printUsage