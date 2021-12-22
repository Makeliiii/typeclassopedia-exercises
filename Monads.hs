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