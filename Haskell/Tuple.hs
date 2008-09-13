module Tuple
       where

data Tuple a b c d = Tuple1 a
                   | Tuple2 a b
                   | Tuple3 a b c
                   | Tuple4 a b c d

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple1 a) = Nothing
tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b

tuple3 (Tuple1 a) = Nothing
tuple3 (Tuple2 a b) = Nothing
tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c

tuple4 (Tuple1 a) = Nothing
tuple4 (Tuple2 a b) = Nothing
tuple4 (Tuple3 a b c) = Nothing
tuple4 (Tuple4 a b c d) = Just d

toValue (Tuple1 a) = Left (Left a)
toValue (Tuple2 a b) = Left (Right (a,b))
toValue (Tuple3 a b c) = Right (Left (a,b,c))
toValue (Tuple4 a b c d) = Right (Right (a,b,c,d))

