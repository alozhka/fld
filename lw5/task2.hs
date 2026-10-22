import Data.Char (isPunctuation)
import System.Environment (getArgs)
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (IOException, catch)

printUsage :: IO ()
printUsage = putStrLn "Usage: task2 <inputFilename> <outputFilename>"

handleIOError :: IOException -> IO ()
handleIOError e = putStrLn ("Error: " ++ show e)

replace :: Char -> String -> String
replace ch = map (\x -> if isPunctuation x then ch else x)

replacePunctuations :: Char -> String -> String -> IO ()
replacePunctuations ch inputFilename outputFilename = do
  text <- readFile' inputFilename
  writeFile outputFilename (replace ch text)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering

  args <- getArgs
  case args of
    [input, output] -> do
      exists <- doesFileExist input
      if not exists
        then putStrLn "Error: input file does not exist"
        else do
          putStr "Replacement character: "
          line <- getLine
          case line of
            [ch] -> (do
              replacePunctuations ch input output
              putStrLn "Done.") `catch` handleIOError
            _ -> putStrLn "Error: a character was expected"
    _ -> printUsage