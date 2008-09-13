module ReFoldr
       where


--and' [] = True
--and' (b:bs) =
--     if not b
--        then False
--        else and' bs

and' = foldr 
             (\b r -> if not b
                         then False
                         else r)
             True
                   

concatMap' f = foldr (\x r -> (f x) ++ r) []
                