{-# LANGUAGE DeriveGeneric #-}
module Capital.Demo.Model where

import           GHC.Generics


import           Data.Text.Lazy as L

type DemoId = Int

data Demo = Demo { demoId          :: DemoId
                 , demoName        :: String
                 , demoDescription :: L.Text
                 } deriving (Show, Generic)

instance ToJSON Demo
instance FromJSON Demo

data DemoView = DemoView { confirmedDemos :: M.Map DemoId Demo } deriving (Show, Generic)

instance BusinessModel DemoView where

  data Command DemoView = AddDemo Demo
                        | UpdateDemo DemoId Demo
                        | DeleteDemo DemoId
                          deriving (Show, Generic)
                                   
  data Event DemoView = DemoAdded Demo
                      | DemoUpdated DemoId Demo
                      | DemoDeleted DemoId
                      | DemoError String
                      deriving (Show, Generic)
  
  (DemoView d) `act` AddDemo d                 = let did = nextId d
                                                 in DemoAdded d { demoId = did }
                                                    
  (DemoView d) `act` UpdateDemo did newDemo    = validate (flip M.member d) did  (DemoUpdated did newDemo) (demoError $ DemoNotFound did)
  (DemoView d) `act` DeleteDemo did            = validate (flip M.member d) did (DemoDeleted did) (demoError $ DemoNotFound did))

  (DemoView d) `apply` DemoAdded demo          = DemoView (M.insert (demoId demo) demo d)
  (DemoView d) `apply` DemoUpdated demoid demo = DemoView (M.adjust (const demo) demoId d))
  (DemoView d) `apply` DemoDeleted demoid      = DemoView (M.delete did d)    


validate :: (a -> Bool) -> a -> e -> e -> e
validate f a success failure | f a = success
                             | otherwise = failure

data DemoError = DemoNotFound DemoId
               | InvalidDemo
                deriving Show

demoError :: DemoError -> Event DemoView
demoError (DemoNotFound demoId) = DemoError ("Demo not found" <> show demoId)
demoError InvalidDemo           = DemoError "Invalid demo" 
