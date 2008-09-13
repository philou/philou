module BinaryTree
       where


data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)

treeSize (Leaf x) = 1
treeSize (Branch left x right) = 1 + treeSize left + treeSize right

elements (Leaf x) = [x]
elements (Branch left x right) = elements left ++ [x] ++ elements right

treeFold f ri (Leaf x) = f ri x
treeFold f ri (Branch left x right) = 
         treeFold f (f (treeFold f ri right) x) left