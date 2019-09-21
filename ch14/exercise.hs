import Data.Foldable
import Data.Monoid

-- exercise 14.1
-- コンパイルするとDuplicate instance declarations
--instance (Monoid a, Monoid b) => Monoid (a, b) where
--  mempty = (mempty, mempty)
--  (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- exercise 14.2
-- コンパイルするとDuplicate instance declarations
--instance Monoid b => Monoid (a -> b) where
--  -- mempty :: (a -> b)
--  mempty = \_ -> mempty
--  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--  f `mappend` g = \x -> f x `mappend` g x

data Maybe' a = Nothing' | Just' a
  deriving (Show, Ord, Eq, Read)

-- 14.3 
instance Foldable Maybe' where
  -- fold :: Monoid a => (Maybe a) -> a
  fold Nothing' = mempty
  fold (Just' x) = x

  -- foldMap :: Monoid a => (a -> b) -> Maybe a -> b
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x

  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ z Nothing' = z
  foldr f z (Just' x) = f x z

  -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl _ z Nothing' = z
  foldl f z (Just' x) = f z x

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  -- pure :: a -> Maybe' a
  pure = Just'

  -- (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _ = Nothing'
  (Just' g) <*> mx = fmap g mx

instance Traversable Maybe' where
  -- traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing' = pure Nothing'
  traverse g (Just' x) = pure Just' <*> g x

-- exercise 14.4
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf = v
  foldr f v (Node l x r) = f x (foldr f (foldr f v r) l)

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl f v (Node l x r) = f (foldl f (foldl f v l) r) x