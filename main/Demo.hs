{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Demo (startDemoService
            ) where

import           Capital.Demo.Library

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Base     (SomeException)
import           Control.Monad.State        as S
import           Control.Monad.Trans.Either

import           Data.Aeson
import           Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Default
import           Data.Maybe
import           Data.Monoid

import           Demo.Model
import           Demo.Service               as Service
import           Demo.State
import           Demo.Store

import           Network.Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant


type DemoAPI = "api" :> "demos"
               :> (Get '[JSON] [Demo]
                :<|> Capture "demoId" DemoId :> Get '[JSON] Demo
                :<|> ReqBody '[JSON] Demo :> Post '[JSON] (Event DemoView)
                :<|> Capture "demoId" DemoId :> ReqBody '[JSON] Demo :> Put '[JSON] (Event DemoView)
                :<|> Capture "demoId" DemoId :> Delete '[JSON] (Event DemoView)
                )

demoHandlers :: (MonadStore store, MonadIO store) => ServerT DemoAPI (ServiceT (Error DemoView) DemoState store)
demoHandlers = Service.getDemos :<|> getDemo :<|> createDemo :<|> updateDemo :<|> deleteDemo

demoServer :: TVar DemoState -> Server DemoAPI
demoServer s = enter toEither demoHandlers
  where toEither :: ServiceT (Error DemoView) DemoState DemoStore  :~> EitherT ServantErr IO
        toEither = Nat $ \ service -> (liftIO (runInStore $ runService service s) >>= \case
                                       Left err -> left $ toServantErr err
                                       Right a -> return a)

toServantErr :: Error DemoView -> ServantErr
toServantErr (DemoNotFound did) = err404 { errBody = B8.pack $ "could not found demo with id: " <> show did }
toServantErr InvalidDemo = err400


run :: TVar DemoState -> Application
run = serve (Proxy :: Proxy DemoAPI) . demoServer

tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

startDemoService :: IO ()
startDemoService = do
  Prelude.putStrLn "Starting server..."
  s <- newTVarIO def
  r <- tryAny $ Warp.run 3000 $ run s
  Prelude.putStrLn ("Server has stopped; reason = " <> show r)
