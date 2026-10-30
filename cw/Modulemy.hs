module Modulemy
  ( ex1,
    ex2,
    ex3,
    ex4,
  )
where

import Data.List (head, last, sortOn)

--- Задания

ex1 :: [Int] -> Int -> [[Int]]
ex1 arr n = do
  let (acc, curr) = foldl aggregate ([], []) arr
   in if null curr
        then acc
        else acc ++ [curr]
  where
    aggregate (acc, curr) x = do
      let currSum = listSum curr + x
      if x > n
        then
          if curr == []
            then (acc ++ [[x]], [])
            else (acc ++ [curr] ++ [[x]], [])
        else
          if currSum <= n || curr == []
            then (acc, curr ++ [x])
            else (acc ++ [curr], [x])

ex2 :: Int -> [[Int]]
ex2 n = do
  let trios = generateTrios n
   in sortOn last (filter (\trio -> any (\other -> triosEqual trio other) trios) trios)
  where
    triosEqual fst snd = fst /= snd && (last fst) == (last snd)

ex3 :: [Int] -> [Int] -> [Int]
ex3 l1 l2
  | length l1 /= length l2 = error "error"
  | otherwise = [i | i <- [0 .. length l1 - 1], (l1 !! i) `mod` 2 /= (l2 !! i) `mod` 2]

ex4 :: FilePath -> FilePath -> IO ()
ex4 inputFile outputFile = do
  content <- readFile inputFile
  let numberedLines = zip [1 ..] (drop 1 (lines content)) -- заголовок в подсчёте строк не учитываю
  let filtered = filter (\(n, line) -> hasDuplicatedEmail (n, line) numberedLines) numberedLines
  writeFile outputFile (unlines (map format filtered))
  where
    hasDuplicatedEmail (n, line) arr = any (\(otherN, otherLine) -> findEmail line == findEmail otherLine && n /= otherN) arr
    format (n, line) = (show n ++ ". " ++ line)

-- Вспомогательные

listSum :: [Int] -> Int
listSum = foldr (\acc x -> acc + x) 0

square :: Int -> Int
square x = x * x

generateTrios :: Int -> [[Int]]
generateTrios n =
  [ [a, b, c, a + b + c]
  | a <- [1 .. 300],
    b <- [1 .. 300],
    c <- [1 .. 300],
    a < b,
    b < c,
    isMultipleOf a n,
    isMultipleOf b n,
    isMultipleOf c n,
    square a + a * b + square b == square c
  ]

isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y
  | y == 0 = error "error"
  | otherwise = x `mod` y == 0

findEmail :: String -> String
findEmail line =
  let splitted = splitBy ';' line
   in if length splitted /= 5
        then error "error"
        else splitted !! 2

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy ch s =
  let (l, s') = break (== ch) s
   in l : case s' of
        [] -> []
        (_ : s'') -> splitBy ch s''
