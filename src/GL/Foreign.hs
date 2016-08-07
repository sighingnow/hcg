module GL.Foreign
    ( -- useful utilities
      peekFrom
    , pokeTo
      -- re-export other foreign functions.
    , Storable(..)
    , alloca
    , allocaBytes
    , with
    , withCString
    , peekCString
    , nullPtr
    , castPtr
    , intPtrToPtr
    ) where

import           Control.Monad          ( liftM2 )
import           Control.Monad.IO.Class ( MonadIO(..), liftIO )
import           Foreign.Ptr
import           Foreign.Marshal.Alloc  ( alloca, allocaBytes )
import           Foreign.Storable
import           Foreign.C.String       ( peekCString, withCString )
import           Foreign.Marshal.Utils  ( with )

-- | Allocate memory, apply this function to the pointer and return the result value there.
peekFrom :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
peekFrom f = liftIO . alloca $ liftM2 (>>) f peek

-- | Allocate memory, put value into there and apply this function to the pointer.
pokeTo :: (MonadIO m, Storable a) => a -> (Ptr a -> IO b) -> m b
pokeTo v f = liftIO . alloca $ liftM2 (>>) (`poke` v) f
