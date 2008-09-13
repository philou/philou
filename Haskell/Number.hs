module Main
       where

import IO

main = do
     hSetBuffering stdin LineBuffering
     numbers <- askForNumbers
     putStrLn ("The sum is " ++ show (foldr (+) 0 numbers))
     putStrLn ("The product is " ++ show (foldr (*) 1 numbers))
     display numbers

askForNumbers = do
     putStrLn "Give me a number (or 0 to stop):"
     numberString <- getLine
     let number = read numberString
     if 0 == number
        then return []
        else do
             otherNumbers <- askForNumbers
             return (number : otherNumbers)

display [] = return ()
display (n:ns) = do
        putStrLn (show(n) ++ " factorial is " ++ show(factorial(n)))
        display ns

factorial 1 = 1
factorial n = n * factorial (n-1)