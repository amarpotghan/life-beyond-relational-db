{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Demo.Store where

import           Capital.EventSourced
import           Control.Monad.State  as S
import           Data.Aeson
import           Data.ByteString.Lazy as BL
import           Data.Maybe

newtype DemoStore a =
  EventStore { runStore :: StateT [ BL.ByteString ] IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState [ BL.ByteString ])

instance MonadStore DemoStore where
  store a = S.modify (++ [(encode a)])
  load a = do
    rs <- execStateT (runStore a) mempty
    return $ fromMaybe mempty $ traverse decode rs

runInStore :: DemoStore a -> IO a
runInStore = (fst <$>) . flip runStateT [] . runStore

