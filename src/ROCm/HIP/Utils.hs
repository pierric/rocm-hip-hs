module ROCm.HIP.Utils where

import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable (Storable (..), peek)

peekHipObject :: FunPtr (Ptr a -> IO ()) -> (ForeignPtr a -> a) -> Ptr (Ptr a) -> IO a
peekHipObject finalizer wrapper ptr = do
  p <- peek ptr
  p <- newForeignPtr finalizer p
  return $ wrapper p

withCStringList :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringList as f = go as (\list -> withArray list f)
  where
    go [] f = f []
    go (a : as) f = withCString a $ \ca -> go as $ \cas -> f (ca : cas)
