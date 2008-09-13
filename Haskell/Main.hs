module Test
       where

main = withIfs

withCase = do
     putStrLn "Please enter your name:"
     name <- getLine
     case name of
          "Simon" -> putStrLn "I think Haskell is a great programming language."
          "John" -> putStrLn "I think Haskell is a great programming language."
          "Phil" -> putStrLn "I think Haskell is a great programming language."
          "Koen" -> putStrLn "Debugging Haskell is fun."
          _ -> putStrLn "I don't know who you are."


withIfs = do
     putStrLn "Please enter your name:"
     name <- getLine
     putStrLn (if name `elem` ["Simon", "John", "Phil"]
                 then "I think Haskell is a great programming language."
                 else if ("Koen" == name) 
                      then "Debugging Haskell is fun."
                      else "I don't know who you are.")