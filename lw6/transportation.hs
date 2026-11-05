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
  (w /= g || f == g) && (g /= c || f == g)
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

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering

  case solve [L, L, L, L] [R, R, R, R] of
    Nothing -> putStrLn "Решение не найдено."
    Just path -> do
      putStrLn "Порядок: [фермер, волк, козёл, капуста]"
      putStrLn "L — левый берег, R — правый берег"
      mapM_ printStep (zip [0 ..] path)
      putStrLn ("Количество переправ: " ++ show (length path - 1))
  where
    printStep (number, state) =
      putStrLn (show number ++ ": " ++ show state)
