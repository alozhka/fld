module Main (main) where

import System.IO

-- 0
do_my_list :: Int -> [Int]
do_my_list n = take n [n..]

-- 1
oddEven :: [a] -> [a]
oddEven (x:y:remaining) = y : x : oddEven remaining
oddEven xs = xs

-- 2
insert :: [a] -> a -> Int -> [a]
insert xs a n | n <= 1  = a : xs
insert [] a _           = [a]
insert (x:xs) a n       = x : insert xs a (n - 1)

-- 3
listSumm :: Num a => [a] -> [a] -> [a]
listSumm (x1:xs1) (x2:xs2) = (x1 + x2) : listSumm xs1 xs2
listSumm xs1 [] = xs1
listSumm [] xs2 = xs2

-- 4
position :: Eq a => [a] -> a -> Int
position list el = go list 1
    where
        go [] _ = 0
        go (x:xs) pos
            | x == el = pos
            | otherwise = go xs (pos + 1)

-- 5
sumF5 :: Int -> Int
sumF5 n
    | n <= 0 = 0
    | otherwise = n + sumF5 (n - 1)

-- 6
sumF6 :: Int -> Int
sumF6 n = go 1
    where
        go i
            | i > n = 0
            | otherwise = (n - i) + go (i + 1)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering

    putStr "N1: "
    n1 <- readLn
    let myList1 = do_my_list n1

    putStr "N2: "
    n2 <- readLn
    let myList2 = do_my_list n2

    putStr "Вставляемый элемент: "
    a <- readLn

    putStr "Позиция: "
    n <- readLn

    putStr "Элемент для поиска: "
    el <- readLn

    -- 0
    putStrLn ("0. do_my_list: " ++ show myList1)
    -- 1
    putStrLn ("1. oddEven: " ++ show (oddEven myList1))
    -- 2
    putStrLn ("2. insert: " ++ show (insert myList1 a n))
    -- 3
    putStrLn ("3. listSumm: " ++ show (listSumm myList1 myList2))
    -- 4
    putStrLn ("4. position: " ++ show (position myList1 el))
    -- 5
    putStrLn ("5. sumF5: " ++ show (sumF5 n1))
    -- 6
    putStrLn ("6. sumF6: " ++ show (sumF6 n1))
