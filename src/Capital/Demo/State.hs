module Capital.Demo.State where

import           Control.Monad.Trans
import qualified Data.Map                     as M
import           Prelude                      hiding (read)
import           Capital.Demo.Library
  
-- | To make sure we can stub the state
data DState s = DState { getDemos :: s -> DemoView
                       , setDemos :: DemoView -> s -> s
                       , getTime :: s -> IO Date
                       }


data DemoState = DemoState { demos      :: DemoView
                           , logger     :: Logger
                           , now        :: IO Date
                           }


prodDemoState :: DState DemoState
prodDemoState = DState { getDemos = demos
                       , setDemos es s = s { demos = es }
                       , getTime = now
                       }

type Logger = String -> IO ()

initDemosState :: Logger -> IO Date -> DemoState
initDemosState = DemoState Library.init

instance HasLog DemoState where
  getLog = logger

instance BusContainer DemoState where
  bus = commandBus

instance CommandExecutor DemoView DemoState LocalState where
  getView       = getDemos prodDemoState
  getType       = const DemoEvent
  getEventType  = const DemoEvent
  setView       = setDemos prodDemoState
  applyCommand  = applyLocalCommand

type DemoService a = forall l m . (MonadIO m) => WebStateM DemoState l m a
type DemoServiceM a = forall l m s . (EventStore m, HasLog s, DState s, CommandExecutor DemoView s l) => WebStateM s l m a


type InDemos a = forall s . DState s -> InSTM s a

runWithDemos :: InDemos a -> DemoServiceM a
runWithDemos f = ask >>= liftIO . atomically . extractInSTM f 

doGet :: (DemosView -> M.Map k a) -> InDemos [ a ]
doGet f = inSTM $ fmap (M.elems . f . getDemos) . readTVar


