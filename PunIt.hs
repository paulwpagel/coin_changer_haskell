module PunIt where

assert x y = if x == y then True else False

fail_message x y = do putStr "Fail: "
                      print x
                      putStr "should have been "
                      print y

assertEqual x y = if assert x y then putStr "." else fail_message x y 
assertEquals x y = if assert x y then putStr "." else fail_message x y 


start = putStrLn "Haskell specs"
end = putStrLn ""