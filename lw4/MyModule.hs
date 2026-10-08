module MyModule (
    splitAt,
    isNumber,
    insert,
    partition,
    delete,
    isAscii,
    span,
    size,
    intToDigit
) where

import Prelude hiding (splitAt, span, size, intToDigit)
import qualified Data.Map as DataMap (Map, fromList, toList)
import qualified Data.Set as DataSet (Set, fromList, toList)

-- 1. splitAt (Data.List)
-- Делит список на две части: первые n элементов и оставшиеся элементы.
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- 2. isNumber (Data.Char)
-- Проверяет, является ли символ цифрой.
isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'

-- 3. insert (Data.Map)
-- Вставляет значение по ключу в Map. Если ключ уже существует, то обновляет его значение.
insert :: Ord k => k -> v -> DataMap.Map k v -> DataMap.Map k v
insert k v map = DataMap.fromList (go (DataMap.toList map))
    where
        go [] = [(k, v)]
        go ((k', v'):xs)
            | k == k'   = (k, v) : xs
            | otherwise = (k', v') : go xs


-- 4. partition (Data.List)
-- Делит список на два списка: один с элементами, удовлетвояющими предикату, и другой с элементами, не удовлетвояющими предикату.
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- 5. delete (Data.Set)
-- Удаляет элемент из множества.
delete :: Ord a => a -> DataSet.Set a -> DataSet.Set a
delete el set = DataSet.fromList (filter (/= el) (DataSet.toList set)) 

-- Допы
-- 6. isAscii (Data.Char)
-- Проверяет, подходит ли символ под ASCII-формат.
isAscii :: Char -> Bool
isAscii c = fromEnum c < 128

-- 7. span (Data.List)
-- Делит список на два списка в точке, в которой условие перестает выполняться.
-- Идет по списку слева направо и забирает элементы в первый список, пока они подходят под условие.
-- Остальные элементы идут во второй список.
span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

-- 8. size (Data.Map)
-- Возвращает количество элементов в Map.
size :: DataMap.Map k v -> Int
size map = length (DataMap.toList map)

-- 9. intToDigit (Data.Char)
-- Преобразует число от 0 до 15 в символ шестнадцатеричной системы счисления.
-- Если число не подходит под диапазон, то выдает ошибку.
intToDigit :: Int -> Char
intToDigit n
    | n >= 0 && n <= 9   = toEnum (n + fromEnum '0')
    | n >= 10 && n <= 15 = toEnum (n - 10 + fromEnum 'a')
    | otherwise          = error ("intToDigit: not a digit " ++ show n)