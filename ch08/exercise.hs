data Nat = Zero | Succ Nat deriving Show

-- 8.1
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)

-- 8.2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case x `compare` y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

-- 8.3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

countLeaf :: Tree' a -> Int
countLeaf (Leaf' x) = 1
countLeaf (Node' l r) = countLeaf l + countLeaf r

balanced :: Tree' a -> Bool
balanced (Leaf' x) = True
balanced (Node' l r) = balanced l && balanced r && abs (countLeaf r - countLeaf l) <= 1

