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