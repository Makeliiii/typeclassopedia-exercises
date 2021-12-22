import Control.Monad (join)

-- some prep with The Trivial Monad
-- http://blog.sigfpe.com/2007/04/trivial-monad.html

newtype W a = W a deriving (Show)

return' :: a -> W a
return' = W

fmap' :: (a -> b) -> W a -> W b
fmap' f (W a) = W (f a)

a = W 1

b = fmap' (+ 1) a

f :: Int -> W Int
f x = W (x + 1)

bind :: (a -> W b) -> W a -> W b
bind f (W x) = f x

-- 1.
g :: Int -> W Int -> W Int
g x = bind (return' . (+) x)

-- 2.
h :: W Int -> W Int -> W Int
h x y = bind (`g` y) x

-- holy shit i'm dumb

-- Exercise 1. Implement a Monad instance for the list constructor, []. Follow the types!
class MyMonad m where
  return'' :: a -> m a
  (>>>=) :: m a -> (a -> m b) -> m b

instance MyMonad [] where
  return'' x = [x]
  (>>>=) xs f = [y | x <- xs, y <- f x]

-- Exercise 2. Implement a Monad instance for ((->) e).
instance MyMonad ((->) e) where
  return'' = const

  -- >>>= :: (e -> a) -> (a -> e -> b) -> e -> b
  -- I think it's like this
  (>>>=) x f e = f (x e) e

-- Exercise 3. Implement Functor and Monad instances for Free f, defined as
-- data Free f a = Var a | Node (f (Free f a))
-- You may assume that f has a Functor instance. This is known as the free monad built from the functor f.

data Free f a = Var a | Node (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Var a) = Var (f a)
  fmap f (Node x) = Node (fmap (fmap f) x)

-- would need to define applicative too if you want to define monad but i'm not going to do it :D

instance Functor f => Monad (Free f) where
  return = Var
  (Var a) >>= f = f a
  (Node x) >>= f = Node (fmap (>>= f) x)

-- Intuition
-- Exercise 1. Implement (>>=) in terms of fmap (or liftM) and join.

(<<=) :: Monad m => m a -> (a -> m b) -> m b
x <<= k = join $ fmap k x

-- Exercise 2. Now implement join and fmap (liftM) in terms of (>>=) and return.