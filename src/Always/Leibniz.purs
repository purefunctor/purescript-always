module Always.Leibniz where

import Data.Identity (Identity(..))

newtype Leibniz :: forall k. k -> k -> Type
newtype Leibniz a b = Leibniz (forall f. f a -> f b)

infix 4 type Leibniz as ~

coe :: forall f a b. (a ~ b) -> f a -> f b
coe (Leibniz f) = f

refl :: forall a. (a ~ a)
refl = Leibniz (\x -> x)

coerce :: forall a b. (a ~ b) -> a -> b
coerce p a = case (coe p (Identity a)) of Identity b -> b

newtype Flip :: forall k l. (k -> l -> Type) -> l -> k -> Type
newtype Flip f a b = Flip (f b a)

unFlip :: forall f a b. Flip f a b -> f b a
unFlip (Flip fba) = fba

symm :: forall a b. (a ~ b) -> (b ~ a)
symm p = unFlip (coe p (Flip refl))
