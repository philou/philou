module Test
       where

x = 5
y = (6, "Hello")
z = x * fst y

square x = x * x

signum x =
       if x < 0
          then -1
          else if x > 0
                  then 1
                  else 0

f x =
    case x of
      0 -> 1
      1 -> 5
      2 -> 2
      _ -> -1

roots a b c =
      let det = sqrt (b*b - 4*a*c)
          twice_a = 2*a
      in ((-b + det) / twice_a,
          (-b - det) / twice_a)

factorial 1 = 1
factorial n = n * factorial (n-1)

my_length [] = 0
my_length (x:xs) = 1 + my_length xs

my_filter predicate [] = []
my_filter predicate (x:xs) =
          if predicate x
             then x : my_filter predicate xs
             else my_filter predicate xs
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

mult a 0 = 0
mult a b = a + mult a (b-1)

my_map f [] = []
my_map f (x:xs) = f x : my_map f xs