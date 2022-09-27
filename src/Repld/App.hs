module Repld.App
  ( App,
    Appx,
    runApp,
    io,
    label,
    With,
    with,
  )
where

import Control.Monad (ap)
import Control.Monad qualified
import Control.Monad.IO.Class (MonadIO (liftIO))
import Prelude hiding (return)

newtype App x r a = App
  { unApp :: r -> (a -> IO x) -> IO x
  }
  deriving stock (Functor)

instance Applicative (App x r) where
  pure x = App \_ k -> k x
  (<*>) = ap

instance Monad (App x r) where
  return = pure
  App mx >>= f =
    App \r k ->
      mx r \a ->
        unApp (f a) r k

instance MonadIO (App x r) where
  liftIO m = App \_ k -> m >>= k

type Appx x r a =
  App (X x a) r a

runApp :: r -> App a r a -> IO a
runApp r (App k) =
  k r pure

runAppx :: r -> Appx x r a -> IO (X x a)
runAppx r (App k) =
  k r (pure . Xr)

return :: x -> App x r a
return x =
  App \_ _ -> pure x

io :: IO a -> App x r a
io =
  liftIO

label :: forall r x a. ((forall xx void. Label (X x a) xx => a -> App xx r void) -> Appx x r a) -> App x r a
label f =
  App \r k -> do
    unX k (runAppx r (f \a -> return (wrap @(X x a) (Xr a))))

type With a =
  forall r. (a -> IO r) -> IO r

with :: With a -> (a -> Appx x r b) -> App x r b
with f action =
  App \r k ->
    unX k (f \a -> runAppx r (action a))

-- instance Label (X a b) (X a b)
-- instance Label (X a b) (X (X a b) c)
-- instance Label (X a b) (X (X (X a b) c) d)
-- etc...

data X a b
  = Xl a
  | Xr b

unX :: (a -> IO b) -> IO (X b a) -> IO b
unX k mx =
  mx >>= \case
    Xl b -> pure b
    Xr a -> k a

class Label a b where
  wrap :: a -> b
  default wrap :: a ~ b => a -> b
  wrap = id

instance Label (X a b) (X a b)

instance {-# OVERLAPPABLE #-} Label a b => Label a (X b c) where
  wrap = Xl . wrap
