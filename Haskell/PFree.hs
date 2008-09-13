module PFree
       where


--func1 x l = map (\y -> y*x) l
func1 x = map (*x)

-- func2 f g l = filter f (map g l)
func2 f g= filter f . map g

func3 f l = l ++ map f l

--func4 l = map (\y -> y+2)
--              (filter (\z -> z `elem` [1..10])
--                      (5:l))

func4 = map (+2) . (filter (flip elem [1..10]) . (5:))

func5 f = foldr (flip $ curry f) 0