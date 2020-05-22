-- | Operations from "System.IO" and "Data.IORef" lifted into effectful contexts
-- using 'Control.Effect.Lift.Lift'. Re-exports operations from
-- "Control.Effect.Exception".
--
-- @since 1.0.0.0

module Control.Effect.Region
  ( -- * Lifted "System.IO"
    Region, Handle, IOMode(..)
  , hOpenFile, hGetLine, hPutStrLn, hIsEof
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
import           Control.Effect.Lift
import           Control.Effect.State
import           Data.IORef (IORef)
import qualified Data.IORef as Ref
import           System.IO (Handle, IOMode)
import qualified System.IO as Sys

-- | @since 0.1.0.0
type Region = State (Ref.IORef [Sys.Handle])

-- | @since 0.1.0.0
hOpenFile :: ( Has Region    sig m
             , Has (Lift IO) sig m
             )
          => Sys.FilePath -> IOMode -> m Sys.Handle
hOpenFile fp mode = do
  h     <- sendIO (Sys.openFile fp mode)
  hs    <- get
  sendIO (modifyIORef hs (h :))
  return h

-- | @since 0.1.0.0
hGetLine :: ( Has Region    sig m
            , Has (Lift IO) sig m
            )
         => Handle -> m String
hGetLine = sendIO . Sys.hGetLine

-- | @since 0.1.0.0
hPutStrLn :: ( Has Region    sig m
             , Has (Lift IO) sig m
             )
          => Handle -> String -> m ()
hPutStrLn h s = sendIO (Sys.hPutStrLn h s)

-- | @since 0.1.0.0
hIsEof :: ( Has Region    sig m
          , Has (Lift IO) sig m
          )
       => Handle -> m Bool
hIsEof = sendIO . Sys.hIsEOF

-- | @since 0.1.0.0
newIORef :: Has (Lift IO) sig m => a -> m (IORef a)
newIORef x = sendIO (Ref.newIORef x)

-- | @since 0.1.0.0
readIORef :: Has (Lift IO) sig m => IORef a -> m a
readIORef x = sendIO (Ref.readIORef x)

-- | @since 0.1.0.0
writeIORef :: Has (Lift IO) sig m => IORef a -> a -> m ()
writeIORef ref x = sendIO (Ref.writeIORef ref x)

-- | @since 0.1.0.0
modifyIORef :: Has (Lift IO) sig m => IORef a -> (a -> a) -> m ()
modifyIORef ref f = sendIO (Ref.modifyIORef ref f)

-- shDup :: RMonadIO m =>SHandle (IORT s (IORT r m)) -> IORT s (IORT r m) (SHandle (IORT r m))
-- ??
