import MyModule
import System.IO
import qualified Data.Map
import qualified Data.Set

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout NoBuffering

  let l1 = [10, 20, 30, 40]
  let l2 = [-2, -5, 0, -1]
  let map = Data.Map.fromList [(1, "one"), (3, "three")]
  let set = Data.Set.fromList [1, 2, 3, 4, 5]

  putStrLn "1. splitAt: "
  print (MyModule.splitAt 3 l1)
  print (MyModule.splitAt 0 l1)
  print (MyModule.splitAt 10 "hello")

  putStrLn "\n2. isNumber: "
  print (MyModule.isNumber '5')
  print (MyModule.isNumber 'a')
  print (MyModule.isNumber ' ')

  putStrLn "\n3. insert: "
  print (MyModule.insert 2 "two" map)
  print (MyModule.insert 1 "ONE" map)
  print (MyModule.insert 5 "five" Data.Map.empty)

  putStrLn "\n4. partition: "
  print (MyModule.partition even l2)
  print (MyModule.partition (> 15) l1)
  print (MyModule.partition (`elem` "aeiou") "haskell")

  putStrLn "\n5. delete: "
  print (MyModule.delete 3 set)
  print (MyModule.delete 10 set)
  print (MyModule.delete 'b' (Data.Set.fromList "abc"))

  putStrLn "\n6. isAscii: "
  print (MyModule.isAscii 'A')
  print (MyModule.isAscii 'é')
  print (MyModule.isAscii 'ж')

  putStrLn "\n7. span: "
  print (MyModule.span (< 0) l2)
  print (MyModule.span (> 100) l1)
  print (MyModule.span (/= ' ') "hello world")
  
  putStrLn "\n8. size: "
  print (MyModule.size map)
  print (MyModule.size (MyModule.insert 2 "two" map))
  print (MyModule.size (Data.Map.empty :: Data.Map.Map Int String))

  putStrLn "\n9. intToDigit: "
  print (MyModule.intToDigit 5)
  print (MyModule.intToDigit 10)
  print (MyModule.intToDigit 15)
