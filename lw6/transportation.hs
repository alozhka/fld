import Data.Set qualified as Set
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

data Bank = L | R
  deriving (Eq, Ord, Show)

-- Порядок элементов: фермер, волк, козёл, капуста.
type State = [Bank]

type Path = [State]

transport :: Bank -> Bank
transport L = R
transport R = L

isRouteSafe :: State -> Bool
isRouteSafe [f, w, g, c] =
  not (wolfCanEatGoat || goatCanEatCabbage)
  where
    wolfCanEatGoat = w == g && farmerNotWithGoat
    goatCanEatCabbage = g == c && farmerNotWithGoat
    farmerNotWithGoat = f /= g
isRouteSafe _ = False

availableTransportations :: State -> [State]
availableTransportations state@[f, w, g, c]
  | isRouteSafe state =
      filter isRouteSafe $
        [[transport f, w, g, c]]
          ++ [[transport f, transport w, g, c] | f == w]
          ++ [[transport f, w, transport g, c] | f == g]
          ++ [[transport f, w, g, transport c] | f == c]
availableTransportations _ = []

solve :: State -> State -> Maybe Path
solve start goal
  | not (isRouteSafe start && isRouteSafe goal) = Nothing
  | otherwise = search [[start]] (Set.singleton start)
  where
    search :: [Path] -> Set.Set State -> Maybe Path
    search [] _ = Nothing
    search (path@(current : _) : queue) visited
      | current == goal = Just (reverse path)
      | otherwise =
          let notVisited = filter (`Set.notMember` visited) (availableTransportations current)
              paths = map (: path) notVisited
              visited' = Set.union visited (Set.fromList notVisited)
           in search (queue ++ paths) visited'

findSolution :: String -> State -> State -> IO ()
findSolution description start goal = do
  putStrLn ("\n" ++ description)
  putStrLn ("Начало: " ++ show start ++ "; цель: " ++ show goal)
  case solve start goal of
    Nothing
      | not (isRouteSafe goal) ->
          putStrLn "Решения нет: конечное состояние небезопасно."
    Nothing -> putStrLn "Решение не найдено."
    Just path -> do
      mapM_ printStep (zip [0..] path)
      putStrLn ("Количество переправ: " ++ show (length path - 1))
  where
    printStep (number, state) =
      putStrLn (show number ++ ": " ++ show state)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering
  putStrLn "Порядок: [фермер, волк, козёл, капуста]"
  putStrLn "L — левый берег, R — правый берег"

  findSolution "a) Перевезти всех на другой берег" [L, L, L, L] [R, R, R, R]
  findSolution "b) Перевезти всех и вернуться одному" [L, L, L, L] [L, R, R, R]
  findSolution "c) Перевезти волка и козла; капуста уже справа" [L, L, L, R] [R, R, R, R]
  findSolution "d) Перевезти волка и козла; капусту оставить слева" [L, L, L, L] [R, R, R, L]
  findSolution "e) Перевезти волка и капусту; козёл уже справа" [L, L, R, L] [R, R, R, R]
