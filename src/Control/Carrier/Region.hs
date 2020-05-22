{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}

module Control.Carrier.Region where

import Control.Carrier.State.Strict
import Control.Effect.Region
import Control.Monad.IO.Class
import Data.IORef
import System.IO
{-
runRegion :: MonadIO io => StateC (IORef [Handle]) io a -> io a
runRegion r = do
  cxt    <- liftIO (newIORef [])
  (s, a) <- runState cxt r
  liftIO (readIORef s >>= mapM_ hClose)
  return a

-- | @since 1.0.0.0
region :: ( MonadIO io
          , Has Region sig io
          )
       => StateC (IORef [Sys.Handle]) io a -> io a
region r = do
  cxt    <- liftIO (newIORef [])
  (s, a) <- runState cxt r
  liftIO (readIORef s >>= mapM_ Sys.hClose)
  return a
-}
