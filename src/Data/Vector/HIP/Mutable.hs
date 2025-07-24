{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.HIP.Mutable where

import Prelude (IO, Int, Bool(..), (==), (*), (-), ($), (.), undefined, Monad(..), Maybe(..), fromIntegral, min, max, fmap, otherwise)
import Data.Vector.Internal.Check
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToPrim)
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc (allocaBytes)
import ROCm.HIP

data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(ForeignPtr HipDeviceptr)

type IOVector a = MVector (PrimState IO) a

unsafeWith :: IOVector a -> (HipDeviceptr -> IO b) -> IO b
unsafeWith (MVector _ fptr) act = withForeignPtr fptr $ act . HipDeviceptr

unsafeSlice :: forall s a. Storable a => Int -> Int -> MVector s a -> MVector s a
unsafeSlice i n (MVector _ fptr) = MVector n (plusForeignPtr fptr offset)
  where
    offset = i * sizeOf (undefined :: a)

unsafeRead :: forall m a. (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
unsafeRead (MVector _ fptr) i = unsafePrimToPrim $ do
  let bytes = sizeOf (undefined :: a)
  allocaBytes bytes $ \(out :: Ptr a) ->
    withForeignPtr fptr $ \ptr -> do
      let ptr_ith = advanceDeviceptr (i * bytes) (HipDeviceptr ptr)
      hipMemcpyDtoH (castPtr out) ptr_ith (fromIntegral bytes)
      peek out

new :: forall m a. (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
new size = unsafePrimToPrim $ do
  let bytes = sizeOf (undefined :: a) * size
  HipDeviceptr ptr <- hipMalloc (fromIntegral bytes)
  fptr <- newForeignPtr hipFreeAsFunPtr ptr
  return $ MVector size fptr

copyToDevice :: forall m a. (PrimMonad m, Storable a) => VSM.MVector (PrimState m) a -> m (MVector (PrimState m) a)
copyToDevice (VSM.MVector n fhptr) = do
  vhip@(MVector _ fdptr) <- new n
  unsafePrimToPrim $ do
    let bytes = sizeOf (undefined :: a) * n
    withForeignPtr fdptr $ \dptr -> 
      withForeignPtr fhptr $ \hptr ->
        hipMemcpyHtoD (HipDeviceptr dptr) (castPtr hptr) (fromIntegral bytes)
  return vhip

copyToHost :: forall m a. (PrimMonad m, Storable a) => MVector (PrimState m) a -> m (VSM.MVector (PrimState m) a)
copyToHost (MVector n fdptr) = do
  vcpu@(VSM.MVector _ fhptr) <- VSM.new n
  unsafePrimToPrim $ do
    let bytes = sizeOf (undefined :: a) * n
    withForeignPtr fdptr $ \dptr -> 
      withForeignPtr fhptr $ \hptr ->
        hipMemcpyDtoH (castPtr hptr) (HipDeviceptr dptr) (fromIntegral bytes)
  return vcpu

replicate :: (PrimMonad m, Storable a) => Int -> a -> m (MVector (PrimState m) a)
replicate n v = do
  vcpu <- VSM.replicate n v
  copyToDevice vcpu

replicateM :: (PrimMonad m, Storable a) => Int -> m a -> m (MVector (PrimState m) a)
replicateM n mv = do
  vcpu <- VSM.replicateM n mv
  copyToDevice vcpu

generate :: (PrimMonad m, Storable a) => Int -> (Int -> a) -> m (MVector (PrimState m) a) 
generate n gen = do
  vcpu <- VSM.generate n gen
  copyToDevice vcpu

generateM :: (PrimMonad m, Storable a) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
generateM n gen = do
  vcpu <- VSM.generateM n gen
  copyToDevice vcpu

clone :: forall m a. (PrimMonad m, Storable a) => MVector (PrimState m) a -> m (MVector (PrimState m) a)
clone (MVector n fiptr) = unsafePrimToPrim $ do
  let bytes = sizeOf (undefined :: a) * n
  HipDeviceptr optr <- hipMalloc (fromIntegral bytes)

  withForeignPtr fiptr $ \iptr -> 
    hipMemcpyDtoD (HipDeviceptr optr)  (HipDeviceptr iptr) (fromIntegral bytes)

  foptr <- newForeignPtr hipFreeAsFunPtr optr
  return $ MVector n foptr

read :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
read v i = checkIndex Bounds i (length v) $ unsafeRead v i

readMaybe :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m (Maybe a)
readMaybe v i | i `inRange` (length v) = fmap Just (unsafeRead v i)
              | otherwise = return Nothing

length :: Storable a => MVector s a -> Int
length (MVector s _) = s

null :: Storable a => MVector s a -> Bool
null (MVector s _) = s == 0

slice :: Storable a
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
slice i n v = checkSlice Bounds i n (length v) $ unsafeSlice i n v 

take :: Storable a
     => Int -- ^ @n@ length
     -> MVector s a
     -> MVector s a
take n v = unsafeSlice 0 (min (max n 0) (length v)) v


drop :: Storable a => Int -> MVector s a -> MVector s a
drop n v = unsafeSlice (min m n') (max 0 (m - n')) v
  where
    n' = max n 0
    m  = length v

splitAt :: Storable a => Int -> MVector s a -> (MVector s a, MVector s a)
splitAt n v = ( unsafeSlice 0 m v
              , unsafeSlice m (max 0 (len - n')) v
              )
    where
      m   = min n' len
      n'  = max n 0
      len = length v

init :: Storable a => MVector s a -> MVector s a
init v = slice 0 (length v - 1) v

tail :: Storable a => MVector s a -> MVector s a
tail v = slice 1 (length v - 1) v

