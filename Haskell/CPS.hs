module CPS
       where

cfold' f res [] = res
cfold' f prevRes (x:xs) = f x prevRes (\y -> cfold' f y xs)

cfold f prevRes list = cfold' (\x aRes cont -> f x (cont aRes)) prevRes list

rfoldl f res [] = res
rfoldl f res (x:xs) = f x (rfoldl f res xs)

rfoldr f res [] = res
rfoldr f res (x:xs) = rfoldr f (f x res) xs

cmap' f [] = []
cmap' f (x:xs) = f x (\y -> y : cmap' f xs)

cmap f list = cmap' (\x cont -> cont (f x)) list

rmap f [] = []
rmap f (x:xs) = f x : (map f xs)




cfilter' f [] = []
cfilter' f list = 
         if p x then

cfilter p list = cfilter' (\x cont -> cont


rfilter p [] = []
rfilter p (x:xs) = 
        if p x then
           (x:rfilter p xs)
        else
           rfilter p xs