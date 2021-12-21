-- simple functor definition
class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

-- Exercise 1.
instance MyFunctor (Either a) where
  fmap' f (Left x) = Left x
  fmap' f (Right x) = Right (f x)

instance MyFunctor ((->) e) where
  fmap' f h = f . h

-- Exercise 2.
instance MyFunctor ((,) e) where
  fmap' f (e, x) = (e, f x)

data Pair a = Pair a a deriving (Show)

instance MyFunctor Pair where
  fmap' f (Pair x y) = Pair (f x) (f y)

-- Explain their similarities and differences
-- I guess they are both pairs so that's in common.
-- As far as differences go, their types are completely different.
-- Pair takes two values of the same type whereas ((,) e) holds the "annotation"
-- of type e along the actual value in the pair.
-- ((,) e) we can apply fmap's f to only one argument but with Pair we can apply f to both arguments

-- Exercise 3.
data ITree a = Leaf (Int -> a) | Node [ITree a]

instance MyFunctor ITree where
  fmap' f (Leaf h) = Leaf (f . h)
  fmap' f (Node xs) = Node (map (fmap' f) xs)

-- Exercise 4.
-- Contravariant data types like:
newtype T a = T (a -> Int)

-- Exercise 5.
-- True
newtype Compose f g x = Compose (f (g x))

instance (MyFunctor f, MyFunctor g) => MyFunctor (Compose f g) where
  fmap' f (Compose x) = Compose ((fmap' . fmap') f x)

-- Functor laws

-- Exercise 1.
data Break a = Yes | No deriving (Eq)

instance MyFunctor Break where
  fmap' f _ = No

-- Exercise 2.
-- Think it's only the first one... not going to give examples it just makes sense in my head :)
