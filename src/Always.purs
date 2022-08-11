module Always where

import Prelude

import Control.Apply (lift2)
import Data.Leibniz (type (~), Leibniz(..), coerce)

newtype Always :: (Type -> Type) -> Type -> Type
newtype Always m a = Always (m a)

derive newtype instance Functor m => Functor (Always m)
derive newtype instance Apply m => Apply (Always m)
derive newtype instance Applicative m => Applicative (Always m)
derive newtype instance Bind m => Bind (Always m)
derive newtype instance Monad m => Monad (Always m)

instance (Apply m, Semigroup a) => Semigroup (Always m a) where
  append = lift2 append

instance (Applicative m, Monoid a) => Monoid (Always m a) where
  mempty = pure mempty

data AlwaysProof :: (Type -> Type) -> (Type -> Type) -> Type
data AlwaysProof a b = Yes (forall c. a c ~ b c) | Yesn't

always :: forall m a. m a -> Always m a
always = Always

yes :: forall a. AlwaysProof a a
yes = Yes (Leibniz identity)

yesn't :: forall a b. AlwaysProof a b
yesn't = Yesn't

unalways :: forall m n a. Monad n => Monoid a => AlwaysProof m n -> Always m a -> n a
unalways = case _ of
  Yes prf -> case _ of
    Always mnd -> coerce prf mnd
  Yesn't -> \_ -> pure mempty
