{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}

module Control.Carrier.Region
  ( RegionC, runRegion, region
    -- * Re-exports
  , module Control.Effect.Region
  ) where

import           Control.Carrier.Reader
import           Control.Effect.Region
import           Control.Monad.IO.Class
import qualified System.IO as Sys

-- | @since 0.1.0.0
type RegionC = ReaderC (IORef [Handle])

-- | @since 0.1.0.0
runRegion :: MonadIO m => RegionC m a -> m a
runRegion r = do
  cxt <- liftIO (newIORef [])
  x   <- runReader cxt r
  liftIO (readIORef cxt >>= mapM_ Sys.hClose)
  return x

-- | @since 1.0.0.0
region :: Has Region sig m => RegionC m a -> m a
region subregion = do
  env <- ask
  runReader env subregion
