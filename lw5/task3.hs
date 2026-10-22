import Data.List (isInfixOf)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

-- вспомогательные функции

printUsage :: IO ()
printUsage = putStrLn "Usage: task3 <file1> [<file2> ...]"

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
deleteLine n ls = [line | (lineNumber, line) <- zip [1 ..] ls, lineNumber /= n]

isSameFile :: FilePath -> FilePath -> IO Bool
isSameFile a b = (==) <$> canonicalizePath a <*> canonicalizePath b

filterBySubstring :: String -> [String] -> [String]
filterBySubstring substr = filter (substr `isInfixOf`)

filterByLength :: Int -> [String] -> [String]
filterByLength n = filter ((> n) . length)

-- функции из меню

view :: FilePath -> IO ()
view file = do
  ls <- readLines file
  if null ls
    then putStrLn "(File is empty)"
    else do
      putStrLn "------(content)------"
      mapM_ putStrLn ls
      putStrLn "---------------------"

append :: FilePath -> IO ()
append file = do
  line <- prompt "Enter line to append: "
  appendFile file (line ++ "\n")
  putStrLn "Appended."

delete :: FilePath -> IO ()
delete file = do
  ls <- readLines file
  if null ls
    then putStrLn "Error: file is empty"
    else do
      str <- prompt "Enter line number to delete: "
      case readMaybe str of
        Just lineNo | 1 <= lineNo && lineNo <= length ls -> do
          writeFile file (unlines (deleteLine lineNo ls))
          putStrLn ("Line " ++ show lineNo ++ " was deleted")
        _ -> putStrLn "Error: Invalid line number"

copyWithFilter :: FilePath -> IO ()
copyWithFilter file = do
  newFile <- prompt "Enter new file name: "
  same <- isSameFile file newFile
  if same
    then putStrLn "Error: new file must differ from the source file"
    else do
      putStrLn "Choose filter:\n1) lines containing a substring\n2) lines more than N characters"
      filterChoice <- prompt "> "

      result <- case filterChoice of
        "1" -> do
          substr <- prompt "Enter substring: "
          return (Just (filterBySubstring substr))
        "2" -> do
          minN <- prompt "Enter N: "
          case readMaybe minN of
            Just n | n >= 0 -> return (Just (filterByLength n))
            _ -> return Nothing
        _ -> return Nothing

      case result of
        Just applyFilter -> do
          ls <- readLines file
          writeFile newFile (unlines (applyFilter ls))
          putStrLn ("Filtered lines copied to " ++ newFile)
        Nothing -> putStrLn "Error: Invalid input"

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
    else do
      file <- chooseFile args
      exists <- doesFileExist file
      if not exists
        then putStrLn ("Warning: file " ++ file ++ " does not exist, it will be created when you append a line")
        else return ()
      menu file