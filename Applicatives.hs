import Control.Applicative

-- Laws
-- Exercise 1. (Tricky) One might imagine a variant of the interchange law that says something about applying a pure function to an
-- effectful argument. Using the above laws, prove that pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- pure (flip ($)) <*> x <*> pure f =
-- yea i can't do this lmao

-- Instances
class Functor f => MyApplicative f where
  pure' :: a -> f a
  (<@>) :: f (a -> b) -> f a -> f b

-- Exercise 1. Make Maybe an instance of MyApplicative
instance MyApplicative Maybe where
  pure' = Just
  (<@>) (Just f) (Just x) = Just (f x)
  (<@>) Nothing _ = Nothing
  (<@>) _ Nothing = Nothing
