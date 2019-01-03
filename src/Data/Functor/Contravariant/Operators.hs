module Data.Functor.Contravariant.Operators
( (>$<)
, (>*<)
, (>*)
, (*<)
, (>|<)
, module Data.Functor.Contravariant
) where

import Data.Functor.Contravariant hiding ((>$<))
import Data.Functor.Contravariant.Divisible (Divisible (divide), Decidable, chosen, divided)

(>$<) :: Contravariant f => (b -> a) -> f a -> f b
(>$<) = contramap
{-# INLINE (>$<) #-}
infixr 3 >$<

(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided
{-# INLINE (>*<) #-}
infixr 4 >*<

(>*) :: Divisible f => f a -> f () -> f a
(>*) a u = divide (\a -> (a,())) a u
{-# INLINE (>*) #-}
infixr 4 >*

(*<) :: Divisible f => f () -> f a -> f a
(*<) u a = divide (\a -> ((),a)) u a
{-# INLINE (*<) #-}
infixr 4 *<

(>|<) :: Decidable f => f a -> f b -> f (Either a b)
(>|<) = chosen
{-# INLINE (>|<) #-}
infixr 3 >|<

