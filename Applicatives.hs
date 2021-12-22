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

-- Exercise 2. Determine the correct definition of pure for the ZipList instance of Applicative—there is only one
-- implementation that satisfies the law relating pure and (<*>).

newtype ZippyList a = ZippyList {getZippyList :: [a]}

instance Functor ZippyList where
  fmap f (ZippyList xs) = ZippyList (map f xs)

instance MyApplicative ZippyList where
  pure' x = ZippyList (repeat x)

  -- (<@>) :: ZippyList (a -> b) -> ZippyList a -> ZippyList b
  (ZippyList gs) <@> (ZippyList xs) = ZippyList (zipWith ($) gs xs)

-- Utility functions

-- Exercise 1. Implement a function
-- sequenceAL :: Applicative f => [f a] -> f [a]
-- There is a generalized version of this, sequenceA, which works for any Traversable (see the later section on Traversable),
-- but implementing this version specialized to lists is a good exercise.

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = foldr (\x -> (<*>) ((:) <$> x)) (pure [])

-- hlint is actually mental

-- Alternative formulation
-- Exercise 1. Implement pure and (<*>) in terms of unit and (**), and vice versa.

class Functor f => Monoidal f where
  unit :: f ()
  (***) :: f a -> f b -> f (a, b)

pure'' :: Monoidal f => a -> f a
pure'' x = fmap (const x) unit

-- (<*@>) :: Monoidal f => f (a -> b) -> f a -> f b
-- (<*@>) gs xs = FUCKING HOW ????!?!?!?!?!?!?!?!??

unit' :: Applicative f => f ()
unit' = pure ()

(**) :: Applicative f => f a -> f b -> f (a, b)
(**) xs ys = fmap (,) xs <*> ys

-- lol dude i have no clue how to do these two exercises :DDDDDDDDD
