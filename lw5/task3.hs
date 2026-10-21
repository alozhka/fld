import System.IO
import System.Environment (getArgs)
import Text.Read (readMaybe)

printUsage :: IO ()
printUsage = putStrLn "Usage: task3 <inputFile>"

prompt :: String -> IO String
prompt msg = putStr msg >> getLine

chooseFile :: [FilePath] -> IO FilePath
chooseFile [f] = return f
chooseFile files = do
    mapM_ (\(index, file) -> putStrLn (show index ++ ") " ++ file)) (zip [1 :: Int ..] files)
    s <- prompt "File number: "
    case readMaybe s of
        Just n | 0 < n && n <= length files -> return (files !! (n - 1))
        _ -> putStrLn "Error: Invalid file number" >> chooseFile files

view :: FilePath -> IO ()
view file = putStrLn ("view " ++ file)

append :: FilePath -> IO ()
append file = putStrLn ("append " ++ file)

delete :: FilePath -> IO ()
delete file = putStrLn ("delete " ++ file)

copy :: FilePath -> IO ()
copy file = putStrLn ("copy " ++ file)

menu :: FilePath -> IO ()
menu file = do
    putStrLn ("File: " ++ file)
    putStrLn ("\nChoose the operation: 1) view 2) append line 3) delete line 4) copy with filter 0) exit")
    choice <- prompt "> "

    case choice of
        "1" -> view file >> menu file
        "2" -> append file >> menu file
        "3" -> delete file >> menu file
        "4" -> copy file >> menu file
        "0" -> putStrLn "Exitting..."
        _ -> putStrLn "Error: Invalid option" >> menu file

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    args <- getArgs

    if null args
        then printUsage
        else chooseFile args >>= menu