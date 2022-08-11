module Always.Hyrule where

import Prelude

import Always (Always, AlwaysProof(..), always, unalways, yes)
import Data.Leibniz (coerce, symm)
import Data.Profunctor (dimap)
import FRP.Event (AnEvent)
import Unsafe.Coerce (unsafeCoerce)

alwaysEvent :: forall m. Monad m => AnEvent m ~> AnEvent (Always m)
alwaysEvent = unsafeUnwrapEvent >>> dimap (map (unalways yes)) (map always <<< always) >>>
  unsafeWrapEvent
  where
  unsafeUnwrapEvent
    :: forall a. AnEvent m a -> ((a -> m Unit) -> m (m Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> Always m Unit) -> Always m (Always m Unit)) -> AnEvent (Always m) a
  unsafeWrapEvent = unsafeCoerce

unalwaysEvent
  :: forall m n. Monad m => Monad n => AlwaysProof m n -> AnEvent (Always m) ~> AnEvent n
unalwaysEvent proof = unsafeUnwrapEvent >>> dimap asAlways asBase >>> unsafeWrapEvent
  where
  asAlways :: forall a. (a -> n Unit) -> a -> Always m Unit
  asAlways = case proof of
    Yes prf -> map (always <<< coerce (symm prf))
    Yesn't -> mempty

  asBase :: Always m (Always m Unit) -> n (n Unit)
  asBase = case proof of
    Yes _ -> map (unalways proof) <<< unalways proof
    Yesn't -> \_ -> pure $ pure unit

  unsafeUnwrapEvent
    :: forall a. AnEvent (Always m) a -> ((a -> Always m Unit) -> Always m (Always m Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> n Unit) -> n (n Unit)) -> AnEvent n a
  unsafeWrapEvent = unsafeCoerce
