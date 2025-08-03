{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ROCm.HIP.Function where

import Control.Algebra (Algebra (..), Has, type (:+:))
import Control.Carrier.State.Strict (StateC, evalState)
import Control.Effect.State (State, get, put)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import ROCm.HIP
import System.Posix.Files (fileExist)

data HipCachedModule = HipCacheModule
  { _file_path :: FilePath,
    _handler :: HipModule,
    _functions :: HashMap String HipFunction
  }

type ModuleCache = HashMap FilePath HipCachedModule

_getOrUpdate :: (Monad m, Hashable k) => HashMap k v -> k -> (Maybe v -> m (v, a)) -> m (HashMap k v, a)
_getOrUpdate mapping key action = do
  (v, a) <- action (M.lookup key mapping)
  return (M.insert key v mapping, a)

_loadModule :: FilePath -> IO HipModule
_loadModule kernel_file = liftIO $ do
  exist <- fileExist kernel_file

  when (not exist) $
    fail $
      "Kernel file '"
        ++ kernel_file
        ++ "' doesn't exist. Compile it with the command: \n"
        ++ "  hipcc -O3 --genco --offload-arch=gfx1100 <kernel>.hip -o <kernel>.hsaco"

  hipModuleLoad kernel_file

_loadFunction :: HipCachedModule -> String -> IO (HipCachedModule, HipFunction)
_loadFunction (HipCacheModule mod_path mod_obj funs) func_name = do
  (funs', f) <- _getOrUpdate funs func_name $ \case
    Just f -> return (f, f)
    Nothing -> do
      f <- hipModuleGetFunction mod_obj func_name
      return (f, f)

  return (HipCacheModule mod_path mod_obj funs', f)

loadFunction :: (MonadFail m, MonadIO m, Has (State ModuleCache) sig m) => String -> String -> m HipFunction
loadFunction kernel_file func_name = do
  cache <- get

  (cache, fun) <- liftIO $ _getOrUpdate cache kernel_file $ \case
    Nothing -> do
      mod_obj <- _loadModule kernel_file
      _loadFunction (HipCacheModule kernel_file mod_obj M.empty) func_name
    Just cached -> do
      _loadFunction cached func_name

  put cache
  return fun

newtype Hip m a = Hip {unHip :: StateC ModuleCache m a} deriving (Functor, Applicative, Monad, MonadTrans, MonadFail, MonadIO)

instance (Algebra sig m) => Algebra (State ModuleCache :+: sig) (Hip m) where
  alg hdl sig ctx = Hip (alg (unHip . hdl) sig ctx)

instance (PrimMonad m) => PrimMonad (Hip m) where
  type PrimState (Hip m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

runHip :: (Monad m) => Hip m a -> m a
runHip = evalState M.empty . unHip
