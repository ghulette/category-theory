-- Based on http://comonad.com/reader/2008/kan-extensions-ii/

{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}

class (Functor f,Functor g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a)
  unit = leftAdjoint id
  
  counit :: f (g a) -> a
  counit = rightAdjoint id
  
  leftAdjoint :: (f a -> b) -> a -> g b
  leftAdjoint f = fmap f . unit
  
  rightAdjoint :: (a -> g b) -> f a -> b
  rightAdjoint f = counit . fmap f
