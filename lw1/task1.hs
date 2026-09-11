main :: IO ()

main = do
    let input = ((1, 'a'), "abc")
    print (snd (fst (input)))