{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Demo.State where

import           Capital.EventSourced as Library
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Default
import           Demo.Model
import           GHC.Generics
import           Prelude              hiding (read)

data DemoState
  = DemoState {
      demos  :: DemoView
    , logger :: Logger
    }

instance Default DemoState where
  def = DemoState Library.init (const $ return ())

type Logger = String -> IO ()
type DemoService a =
  forall m . (MonadStore m, MonadIO m) => ServiceT (Error DemoView) DemoState m a

initDemosState :: Logger -> DemoState
initDemosState = DemoState Library.init

instance EventClassifier DemoState where
  data EventType DemoState = DemoEvent
    deriving (Enum, Show, Eq, Read, Generic)

  applyEvent demoState (StoredEvent _ DemoEvent _ _ bs) =
    let mev = decode bs :: Maybe (Event DemoView)
    in case mev of
         Just ev -> demoState { demos = demos demoState `apply` ev }
         Nothing -> error $ ("could not parse event for DemoView" ++ show bs)

instance ToJSON (EventType DemoState)
instance FromJSON (EventType DemoState)

instance CommandExecutor DemoView DemoState where
  getView       = demos
  getEventType  = const DemoEvent
  setView  a s  = s { demos = a }
