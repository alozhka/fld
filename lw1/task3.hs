main :: IO ()

main = do
    -- арифметическая прогрессия
    let n = take 20 [1,3..]
    print n

    -- генерация по формуле
    let n = [2*n - 1 | n <- [1..20]]
    print n

    -- итерация
    let n = take 20 (iterate (+2) 1)
    print n