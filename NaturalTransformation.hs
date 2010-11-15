{-# LANGUAGE MultiParamTypeClasses #-}

-- A natural transformation is a structure-preserving mapping from one functor
-- to another.  For Haskell's Functor typeclass, they look like the following.

class (Functor f, Functor g) => NaturalTransform f g where
  eta :: f a -> g a
  -- A valid instance of eta must obey:
  -- fmap f . eta == eta . fmap f
  -- for all f :: a -> b

instance NaturalTransform Maybe [] where
  eta (Just x) = [x]
  eta Nothing = []

instance NaturalTransform [] Maybe where
  eta [] = Nothing
  eta xs = Just (head xs)

-- Example main function
main :: IO ()
main = do
  let f = even
  let x = (fmap f . eta) (Just 1) :: [Bool]
  let y = (eta . fmap f) [1..10] :: Maybe Bool
  print x
  print y
