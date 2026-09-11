main :: IO ()

main = do
    let input = ['a', 'b', 'c']
    print (head (tail input))

    let input = [['a', 'b'], ['c','d']]
    print (head (tail (head input)))

    let input = [['a', 'c', 'd'], ['a','b']]
    print (head (tail (head (tail input))))

    let input = [['a','d'], ['b', 'c']]
    print ( head( head (tail input)))