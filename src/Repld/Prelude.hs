module Repld.Prelude
  ( module X,
    module Repld.Prelude,
  )
where

import Control.Monad as X (forever, when)
import Control.Monad.IO.Class as X (liftIO)
import Data.Bifunctor
import Data.ByteString as X (ByteString)
import Data.Function as X ((&))
import Data.Text as X (Text)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft =
  first

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mx f =
  mx >>= \case
    Nothing -> pure ()
    Just x -> f x

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mx = do
  b <- mb
  when b mx
