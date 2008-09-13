module List
       where


data List a = Nil
            | Cons a (List a)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead (Cons x xs) = x

listTail (Cons x xs) = xs

listFoldl f result Nil = result
listFoldl f result (Cons x xs) = listFoldl f (f result x) xs

listFoldr f result Nil = result
listFoldr f result (Cons x xs) = f x (listFoldr f result xs)


