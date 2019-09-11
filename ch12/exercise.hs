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

-- exercise 12.3
--instance Applicative ((->) a) where
--  pure x = (\_ -> x)
--  f <*> g = \x -> f x (g x)

-- exercise 12.4
newtype ZipList' a = Z' [a] deriving Show
instance Functor ZipList' where
  -- fmap :: (a -> b) -> ZipList' a -> ZipList' b
  fmap g (Z' xs) = Z' (fmap g xs)

instance Applicative ZipList' where
  -- pure :: a -> ZipList' a
  pure x = Z' (repeat x)

  -- (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
  (Z' gs) <*> (Z' xs) = Z' [g x | (g, x) <- zip gs xs]