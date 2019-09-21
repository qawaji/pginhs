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
  