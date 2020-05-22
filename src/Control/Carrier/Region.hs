{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}

module Control.Carrier.Region
  ( RegionC, runRegion, region
    -- * Re-exports
  , module Control.Effect.Region
  ) where

import           Control.Carrier.State.Strict
import           Control.Effect.Region
import           Control.Monad.IO.Class
import qualified System.IO as Sys

-- | @since 0.1.0.0
type RegionC = StateC (IORef [Handle])

-- | @since 0.1.0.0
runRegion :: MonadIO m => RegionC m a -> m a
runRegion r = do
  cxt    <- liftIO (newIORef [])
  (s, a) <- runState cxt r
  liftIO (readIORef s >>= mapM_ Sys.hClose)
  return a

-- | @since 1.0.0.0
region :: Has Region sig m => RegionC m a -> m a
region r = do
  env    <- get
  (s, a) <- runState env r
  return a
