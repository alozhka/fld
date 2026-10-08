module MyModule (
    splitAt,
    isNumber,
    insert,
    partition,
    delete,
    isAscii,
    span,
    size,
    intToDigit,
    splitAt',
    partition',
    size'
) where

import Prelude hiding (splitAt, span, size, intToDigit)
import qualified Data.Map as DataMap (Map, fromList, toList, foldr)
import qualified Data.Set as DataSet (Set, fromList, toList)
import Data.Char (generalCategory, GeneralCategory(..))

-- 1. splitAt (Data.List)
-- Тип:
-- * принимает число n и список элементов любого типа a,
-- * возвращает кортеж из пары списков того же типа.
-- Описание:
-- * Делит список на две части: первые n элементов и оставшиеся элементы.
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- 2. isNumber (Data.Char)
-- Тип:
-- * принимает символ типа Char,
-- * возвращает логическое значение.
-- Описание:
-- * Проверяет, является ли символ числовым символом Unicode.
-- * Подходят цифры, дроби, степени, римские цифры, цифры других письменностей.
isNumber :: Char -> Bool
isNumber c = generalCategory c `elem` [DecimalNumber, LetterNumber, OtherNumber]

-- 3. insert (Data.Map)
-- Тип:
-- * принимает ключ типа k, значение типа v и словарь Map k v,
-- * возвращает новый словарь того же типа.
-- * Ключи должны быть сравнимыми (класс Ord).
-- Описание:
-- * Вставляет значение по ключу в Map. Если ключ уже существует, то обновляет его значение.
insert :: Ord k => k -> v -> DataMap.Map k v -> DataMap.Map k v
insert k v map = DataMap.fromList (go (DataMap.toList map))
    where
        go [] = [(k, v)]
        go ((k', v'):xs)
            | k == k'   = (k, v) : xs
            | otherwise = (k', v') : go xs


-- 4. partition (Data.List)
-- Тип:
-- * принимает предикат (функцию a -> Bool) и список элементов любого типа a,
-- * возвращает кортеж из пары списков того же типа.
-- Описание:
-- * Делит список на два списка: один с элементами, удовлетворяющими предикату, и другой с элементами, не удовлетворяющими предикату.
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- 5. delete (Data.Set)
-- Тип:
-- * принимает элемент типа a и множество Set a,
-- * возвращает новое множество того же типа.
-- * Элементы должны быть сравнимыми (класс Ord).
-- Описание:
-- * Удаляет элемент из множества.
delete :: Ord a => a -> DataSet.Set a -> DataSet.Set a
delete el set = DataSet.fromList (filter (/= el) (DataSet.toList set)) 

-- Допы
-- 6. isAscii (Data.Char)
-- Тип:
-- * принимает символ,
-- * возвращает логическое значение.
-- Описание:
-- * Проверяет, подходит ли символ под ASCII-формат.
isAscii :: Char -> Bool
isAscii c = fromEnum c < 128

-- 7. span (Data.List)
-- Тип:
-- * принимает предикат (функцию a -> Bool) и список элементов любого типа a,
-- * возвращает кортеж из пары списков того же типа.
-- Описание:
-- * Делит список на два списка в точке, в которой условие перестает выполняться.
-- * Идет по списку слева направо и забирает элементы в первый список, пока они подходят под условие.
-- * Остальные элементы идут во второй список.
span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

-- 8. size (Data.Map)
-- Тип:
-- * принимает словарь Map k v с ключами типа k и значениями типа v,
-- * возвращает целое число.
-- Описание:
-- * Возвращает количество элементов в Map.
size :: DataMap.Map k v -> Int
size map = length (DataMap.toList map)

-- 9. intToDigit (Data.Char)
-- Тип:
-- * принимает целое число,
-- * возвращает символ.
-- Описание:
-- * Преобразует число от 0 до 15 в символ шестнадцатеричной системы счисления.
-- * Если число не подходит под диапазон, то выдает ошибку.
intToDigit :: Int -> Char
intToDigit n
    | n >= 0 && n <= 9   = toEnum (n + fromEnum '0')
    | n >= 10 && n <= 15 = toEnum (n - 10 + fromEnum 'a')
    | otherwise          = error ("intToDigit: not a digit " ++ show n)

-- Альтернативные реализации

-- 1'. splitAt через рекурсию
-- * Основная версия проходит список дважды (take и drop).
-- * Здесь список проходится один раз: отщепляем голову и рекурсивно делим хвост.
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' _ []          = ([], [])
splitAt' n (x:xs)      = (x : before, after)
    where
        (before, after) = splitAt' (n - 1) xs

-- 4'. partition через foldr
-- * Основная версия проходит список дважды и вычисляет предикат для каждого элемента 2 раза.
-- * Здесь за один проход каждый элемент кладется в нужный список аккумулятора.
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p = foldr select ([], [])
    where
        select x ~(yes, no)
            | p x       = (x : yes, no)
            | otherwise = (yes, x : no)

-- 8'. size через свертку словаря
-- * Основная версия сначала превращает словарь в список, а затем считает его длину.
-- * Здесь словарь сворачивается напрямую: каждое значение увеличивает счетчик на 1.
size' :: DataMap.Map k v -> Int
size' map = DataMap.foldr (\_ count -> count + 1) 0 map