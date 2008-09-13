module Main
       where


data Triple a b c = Triple a b c

tripleFst (Triple x y z) = x
tripleSnd (Triple x y z) = y
tripleThr (Triple x y z) = z

data Quadruple a b = Quadruple a a b b

firstTwo (Quadruple x y z t) = [x,y]
lastTwo (Quadruple x y z t) = [z,t]
