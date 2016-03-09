{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Demo.State where

import           Capital.Demo.Library as Library
import           Demo.Model
import           Data.Aeson
import           Data.Default
import           GHC.Generics
import           Prelude              hiding (read)
-- | To make sure we can stub the state
data DState s = DState { getDemos :: s -> DemoView
                       , setDemos :: DemoView -> s -> s
                       }

instance Default DemoState where
  def = DemoState Library.init (const $ return ())

data DemoState = DemoState { demos  :: DemoView
                           , logger :: Logger
                           }


prodDemoState :: DState DemoState
prodDemoState = DState { getDemos = demos
                       , setDemos = \ es s -> s { demos = es }
                       }

type Logger = String -> IO ()

initDemosState :: Logger -> DemoState
initDemosState = DemoState Library.init

instance EventClassifier DemoState where
  data EventType DemoState = DemoEvent deriving (Enum, Show, Eq, Read, Generic)

  applyEvent demoState (StoredEvent _ DemoEvent _ _ bs) = let mev = decode bs :: Maybe (Event DemoView)
                                                          in case mev of
                                                              Just ev -> demoState { demos = demos demoState `apply` ev }
                                                              Nothing -> error $ ("could not parse event for DemoView" ++ show bs)

instance ToJSON (EventType DemoState)
instance FromJSON (EventType DemoState)

instance CommandExecutor DemoView DemoState where
  getView       = getDemos prodDemoState
  getType       = const DemoEvent
  getEventType  = const DemoEvent
  setView       = setDemos prodDemoState

type DemoService a = forall m . (MonadStore m) => ServiceT (Error DemoView) DemoState m a

-- type InDemos a = forall s . DState s -> InSTM s a

-- runWithDemos :: InDemos a -> DemoServiceM a
-- runWithDemos f = ask >>= liftIO . atomically . extractInSTM f

-- doGet :: (DemosView -> M.Map k a) -> InDemos [ a ]
-- doGet f = inSTM $ fmap (M.elems . f . getDemos) . readTVar


