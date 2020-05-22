-- | Operations from "System.IO" and "Data.IORef" lifted into effectful contexts
-- using 'Control.Effect.Lift.Lift'. Re-exports operations from
-- "Control.Effect.Exception".
--
-- @since 1.0.0.0

module Control.Effect.Region
  ( -- * Lifted "System.IO"
    Region, hOpenFile, hGetLine, hPutStrLn, hIsEof
    -- * Lifted "Data.IORef"
  , IORef, newIORef, readIORef, writeIORef, modifyIORef
    -- * Exceptions
  , throwIO, throwTo, catch
    -- * Re-exports
  , Algebra
  , Effect
  , Has
  , Lift(..)
  , run

  ) where

import           Control.Algebra
import           Control.Effect.Exception
import           Control.Effect.State
import           Control.Monad.IO.Class
import           Data.IORef (IORef)
import qualified Data.IORef as Ref
import qualified System.IO as Sys

-- | @since 0.1.0.0
type Region = State (Ref.IORef [Sys.Handle])

-- | @since 0.1.0.0
hOpenFile :: (MonadIO m, Has Region sig m)
          => Sys.FilePath -> Sys.IOMode -> m Sys.Handle
hOpenFile fp mode = do
  h     <- liftIO (Sys.openFile fp mode)
  hs    <- get
  liftIO (modifyIORef hs (h :))
  return h

-- | @since 0.1.0.0
hGetLine :: ( MonadIO m, Functor n
            , Has Region   sig m
            , Has (Lift n) sig m
            )
         => n Sys.Handle -> m String
hGetLine h = sendM h >>= liftIO . Sys.hGetLine

-- | @since 0.1.0.0
hPutStrLn :: ( MonadIO m, Functor n
            , Has Region   sig m
            , Has (Lift n) sig m
            )
          => n Sys.Handle -> n String -> m ()
hPutStrLn h str = do
  h' <- sendM str
  s' <- sendM h
  liftIO (Sys.hPutStrLn s' h')

-- | @since 0.1.0.0
hIsEof :: ( MonadIO m, Functor n
          , Has Region   sig m
          , Has (Lift n) sig m
          )
       => n Sys.Handle -> m Bool
hIsEof h = sendM h >>= liftIO . Sys.hIsEOF

-- | @since 0.1.0.0
newIORef :: MonadIO m => a -> m (IORef a)
newIORef x = liftIO (Ref.newIORef x)

-- | @since 0.1.0.0
readIORef :: MonadIO m => IORef a -> m a
readIORef x = liftIO (Ref.readIORef x)

-- | @since 0.1.0.0
writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef ref x = liftIO (Ref.writeIORef ref x)

-- | @since 0.1.0.0
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef ref f = liftIO (Ref.modifyIORef ref f)

-- shDup :: RMonadIO m =>SHandle (IORT s (IORT r m)) -> IORT s (IORT r m) (SHandle (IORT r m))
-- ??
