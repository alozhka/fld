import Control.Monad (forM_)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
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

readLines :: FilePath -> IO [String]
readLines file = do
  exists <- doesFileExist file
  if exists
    then fmap lines (readFile' file)
    else return []

deleteLine :: Int -> [String] -> [String]
deleteLine n lines = [line | (lineNumber, line) <- zip [1 ..] lines, lineNumber /= n]

filterBySubstring :: String -> [String] -> [String]
filterBySubstring substr = filter (substr `isInfixOf`)

filterByLength :: Int -> [String] -> [String]
filterByLength n = filter ((> n) . length)

-- функции из меню
view :: FilePath -> IO ()
view file = do
  lines <- readLines file
  if null lines
    then putStrLn "(File is empty)"
    else do
      putStrLn "------(content)------"
      mapM_ (\l -> putStrLn l) lines
      putStrLn "---------------------"

append :: FilePath -> IO ()
append file = do
  line <- prompt "Enter line to append: "
  appendFile file (line ++ "\n")
  putStrLn "Appended."

delete :: FilePath -> IO ()
delete file = do
  lines <- readLines file
  str <- prompt "Enter line number to delete: "
  case readMaybe str of
    Just lineNo | 1 <= lineNo && lineNo <= length lines -> do
      writeFile file (unlines (deleteLine lineNo lines))
      putStrLn ("Line " ++ show lineNo ++ " was deleted")
    _ -> putStrLn "Error: Invalid line number" >> delete file

copyWithFilter :: FilePath -> IO ()
copyWithFilter file = do
  newFile <- prompt "Enter new file name: "
  putStrLn "Choose filter:\n1) lines containing a substring\n2) lines more than N characters"
  filterChoice <- prompt "> "
  lines <- readLines file

  result <- case filterChoice of
    "1" -> do
      substr <- prompt "Enter substring: "
      return (Just (filterBySubstring substr lines))
    "2" -> do
      maxN <- prompt "Enter max N: "
      return (Just (filterByLength (read maxN) lines))
    _ -> return Nothing

  case result of
    Just filteredLines -> do
      writeFile newFile (unlines filteredLines)
      putStrLn ("Filtered lines copied to " ++ newFile)
    Nothing -> putStrLn "Error: Invalid filter choice" >> copyWithFilter file

menu :: FilePath -> IO ()
menu file = do
  putStrLn ("\nFile: " ++ file)
  putStrLn ("Choose the operation: 1) view 2) append line 3) delete line 4) copy with filter 0) exit")
  choice <- prompt "> "

  case choice of
    "1" -> view file >> menu file
    "2" -> append file >> menu file
    "3" -> delete file >> menu file
    "4" -> copyWithFilter file >> menu file
    "0" -> putStrLn "Exiting..."
    _ -> putStrLn "Error: Invalid option" >> menu file

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering

  args <- getArgs

  if null args
    then printUsage
    else chooseFile args >>= menu