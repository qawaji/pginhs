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

-- exercise 12.6
--instance Monad ((->) a) where
--  return x = (\_ -> x)
--  g >>= f = \x -> f (g x) x

-- exercise 12.7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
-- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap f (Val x) = Val x
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
--  pure :: a -> Expr a
  pure x = Var x

-- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Var f) <*> e = fmap f e
  (Add l r) <*> e = Add ((<*>) l e) ((<*>) r e)

instance Monad Expr where
-- return :: a -> Expr a
  return x = Var x

-- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Val x) >>= _ = (Val x)
  (Var x) >>= f = f x
  (Add l r) >>= f = (Add ((>>=) l f) ((>>=) r f))
