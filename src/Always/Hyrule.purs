module Always.Hyrule where

import Prelude

import Always (Always, always, unalways, yes, yesn't)
import Data.Profunctor (dimap)
import FRP.Event (AnEvent)
import Unsafe.Coerce (unsafeCoerce)

alwaysEventYes :: forall m. Monad m => AnEvent m ~> AnEvent (Always m)
alwaysEventYes = unsafeUnwrapEvent >>> dimap (map (unalways yes)) (map always <<< always) >>>
  unsafeWrapEvent
  where
  unsafeUnwrapEvent
    :: forall a. AnEvent m a -> ((a -> m Unit) -> m (m Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> Always m Unit) -> Always m (Always m Unit)) -> AnEvent (Always m) a
  unsafeWrapEvent = unsafeCoerce

alwaysEventYesn't :: forall m n. Monad m => Monad n => AnEvent m ~> AnEvent (Always n)
alwaysEventYesn't = unsafeUnwrapEvent >>> dimap (map (unalways yesn't)) mempty >>>
  unsafeWrapEvent
  where
  unsafeUnwrapEvent
    :: forall a. AnEvent m a -> ((a -> m Unit) -> m (m Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> Always n Unit) -> Always n (Always n Unit)) -> AnEvent (Always n) a
  unsafeWrapEvent = unsafeCoerce

unalwaysEventYes
  :: forall m
   . Monad m
  => AnEvent (Always m) ~> AnEvent m
unalwaysEventYes = unsafeUnwrapEvent
  >>> dimap (map always) (map (unalways yes) <<< unalways yes)
  >>> unsafeWrapEvent
  where
  unsafeUnwrapEvent
    :: forall a. AnEvent (Always m) a -> ((a -> Always m Unit) -> Always m (Always m Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> m Unit) -> m (m Unit)) -> AnEvent m a
  unsafeWrapEvent = unsafeCoerce

unalwaysEventYesn't
  :: forall m n
   . Monad m
  => Monad n
  => AnEvent (Always m) ~> AnEvent n
unalwaysEventYesn't = unsafeUnwrapEvent
  >>> dimap (map (\_ -> pure mempty)) (map (unalways yesn't) <<< unalways yesn't)
  >>> unsafeWrapEvent
  where
  unsafeUnwrapEvent
    :: forall a. AnEvent (Always m) a -> ((a -> Always m Unit) -> Always m (Always m Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> n Unit) -> n (n Unit)) -> AnEvent n a
  unsafeWrapEvent = unsafeCoerce
