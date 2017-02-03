{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

#define __LOC__ (__FILE__ ++ ":" ++ show (__LINE__ :: Int))

module Main where

import           GHC.Exts               (IsList (..))

import           Data.Hashable          (Hashable)

import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map

import           Data.Sequence          (Seq, ViewL (..), ViewR (..), (<|))
import qualified Data.Sequence          as Seq

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer

newtype Queue a where
  MkQueue :: Seq a -> Queue a
  deriving (Eq, Show, Read)

pushQ :: a -> Queue a -> Queue a
pushQ x (MkQueue xs) = MkQueue (x <| xs)

popQ :: Queue a -> Maybe (Queue a, a)
popQ (MkQueue s) = case Seq.viewr s
                   of Seq.EmptyR -> Nothing
                      xs :> x    -> Just (MkQueue xs, x)

pattern PopQ xs x <- (popQ -> Just (xs, x))

type Map k v = HashMap k v

newtype Addr where
  MkAddr :: Int -> Addr
  deriving (Eq, Ord, Hashable, Show, Read)

newtype Value where
  MkValue :: Int -> Value
  deriving (Eq, Show, Read)

data BState where
  BInvalid  ::           BState
  BShared   :: !Value -> BState
  BModified :: !Value -> BState
  deriving (Eq, Show, Read)

data ProcMessage where
  PLoad  :: !Addr           -> ProcMessage
  PStore :: !Addr -> !Value -> ProcMessage
  PEvict :: !Addr           -> ProcMessage
  deriving (Eq, Show, Read)

data BusMessage where
  BGetS    :: !Addr           -> BusMessage
  BGetX    :: !Addr           -> BusMessage
  BUpgrade :: !Addr           -> BusMessage
  BSource  :: !Addr -> !Value -> BusMessage
  BFlush   :: !Addr -> !Value -> BusMessage
  deriving (Eq, Show, Read)

type MemoryState = Map Addr Value
type CacheState = Map Addr BState

newtype CacheT m a where
  CacheT :: StateT CacheState m a -> CacheT m a
  deriving (Functor, Applicative, Monad, MonadState CacheState, MonadTrans)

type BusT m a = CacheT (StateT (Queue BusMessage) m) a

type CacheM a = CacheT Identity a
type BusM   a = BusT   Identity a

getState    addr       = Map.lookup addr <$> get
setState    addr bs    = modify $ Map.insert addr bs
setInvalid  addr       = setState addr BInvalid
setShared   addr value = setState addr (BShared value)
setModified addr value = setState addr (BModified value)
sendBus     msg        = lift $ modify (pushQ msg)
sendGetS    addr       = sendBus (BGetS    addr)
sendGetX    addr       = sendBus (BGetX    addr)
sendUpgrade addr       = sendBus (BUpgrade addr)
sendSource  addr value = sendBus (BSource  addr value)
sendFlush   addr value = sendBus (BFlush   addr value)

getState     :: (Monad m) => Addr           -> CacheT m (Maybe BState)
setState     :: (Monad m) => Addr -> BState -> CacheT m ()
setInvalid   :: (Monad m) => Addr           -> CacheT m ()
setShared    :: (Monad m) => Addr -> Value  -> CacheT m ()
setModified  :: (Monad m) => Addr -> Value  -> CacheT m ()
sendBus      :: (Monad m) => BusMessage     -> BusT m ()
sendGetS     :: (Monad m) => Addr           -> BusT m ()
sendGetX     :: (Monad m) => Addr           -> BusT m ()
sendUpgrade  :: (Monad m) => Addr           -> BusT m ()
sendSource   :: (Monad m) => Addr -> Value  -> BusT m ()
sendFlush    :: (Monad m) => Addr -> Value  -> BusT m ()

processor :: ProcMessage -> BusM ()
processor (PLoad  addr)     = do Just s <- getState addr
                                 case s of BInvalid    -> sendGetS addr
                                           BShared   _ -> pure ()
                                           BModified _ -> pure ()
processor (PStore addr val) = do Just s <- getState addr
                                 setModified addr val
                                 case s of BInvalid    -> sendGetX addr
                                           BShared   _ -> sendUpgrade addr
                                           BModified _ -> pure ()
processor (PEvict addr)     = do Just s <- getState addr
                                 setInvalid addr
                                 case s of BInvalid    -> fail __LOC__
                                           BShared   _ -> pure ()
                                           BModified v -> sendFlush addr v

--  BGetS    :: !Addr           -> BusMessage
--  BGetX    :: !Addr           -> BusMessage
--  BUpgrade :: !Addr           -> BusMessage
--  BSource  :: !Addr -> !Value -> BusMessage
--  BFlush   :: !Addr -> !Value -> BusMessage

procCache :: BusMessage -> BusM ()
procCache _ = _

cacheCtl :: BusMessage -> BusM ()
cacheCtl _ = _

main :: IO ()
main = pure ()
