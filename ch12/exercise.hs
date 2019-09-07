-- exercise 12.1
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

testTree = Node (Node Leaf 10 Leaf) 20 (Leaf)

-- exercise 12.2
-- Duplicate instance declarationsになる
--instance Functor ((->) a) where
--  fmap = (.)