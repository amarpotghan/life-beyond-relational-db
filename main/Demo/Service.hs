{-# LANGUAGE RankNTypes #-}

module Demo.Service where

import           Capital.EventSourced
import           Demo.Model
import           Demo.State

import           Control.Concurrent.STM.TVar (TVar, readTVar)
import           Control.Monad.Except        (throwError)
import           Control.Monad.Trans      (liftIO)
import           Control.Monad.Reader        (ask)
import           Control.Monad.STM           (STM, atomically)
import qualified Data.Map                    as M

type InSTM s a = TVar s -> STM a

createDemo :: Demo -> DemoService (Event DemoView)
createDemo = applyCommand . AddDemo

updateDemo :: DemoId -> Demo -> DemoService (Event DemoView)
updateDemo did = applyCommand . UpdateDemo did

deleteDemo :: DemoId -> DemoService (Event DemoView)
deleteDemo = applyCommand . DeleteDemo

getDemo :: DemoId -> DemoService Demo
getDemo did = maybe (throwError $ DemoNotFound did) return =<< get (demo did)

getDemos :: DemoService [Demo]
getDemos = get allDemos

get :: InSTM DemoState a -> DemoService a
get instm = ask >>= (liftIO . atomically . instm)

demo :: DemoId -> InSTM DemoState (Maybe Demo)
demo did v =  (M.lookup did . confirmedDemos . demos) <$> readTVar v

allDemos :: InSTM DemoState [Demo]
allDemos v = (M.elems . confirmedDemos . demos)  <$> readTVar v
