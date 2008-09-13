module Monads
    where


# three monadic laws
# 1) return a >>= f === f a
# 2) f >>= return === f
# 3) f >>= (\x -> g x >>= h) === (f >>= g) >>= h

# avec Maybe
instance Monad Maybe where
    return a = Just a
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

1)
Just a >>= f
f a
2)
f >>= Just
si f est Nothing :
   Nothing >>= Just
   Nothing
   f
si f est Just x
   Just x >>= Just
   Just x
   f
3)
si f est Nothing
f >>= (\x -> g x >>= h) <=> Nothing <=> (Nothing >>= g) <=> (Nothing >>=g) >>= h
si f est Just y
f >>= (\x -> g x >>= h)
 <=> Just y >>= (\x -> g x >>= h)
 <=> (\x -> g x >>= h) y 
 <=> g y >>= h

(f >>= g) >>= h
 <=> (Just y >>= g) >>= h
 <=> (g y) >>= h

CQFD

