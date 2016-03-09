{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Capital.Demo.Model where

import           GHC.Generics


import           Capital.Demo.Library
import           Data.Aeson           as A
import           Data.Default
import           Data.Map             as M
import           Data.Text.Lazy       as L
type DemoId = Int

data Demo = Demo { demoId          :: DemoId
                 , demoName        :: String
                 , demoDescription :: L.Text
                 } deriving (Show, Generic, Eq)

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

                      deriving (Show, Generic)

  data Error DemoView = DemoNotFound DemoId
                      | InvalidDemo deriving (Show, Generic)

  init = DemoView M.empty
  (DemoView d) `act` AddDemo demo                 = let did = nextId d
                                                    in return $ DemoAdded demo { demoId = did }

  (DemoView d) `act` UpdateDemo did newDemo    = validate (flip M.member d) did  (DemoUpdated did newDemo) (DemoNotFound did)
  (DemoView d) `act` DeleteDemo did            = validate (flip M.member d) did (DemoDeleted did) (DemoNotFound did)

  (DemoView d) `apply` DemoAdded demo          = DemoView (M.insert (demoId demo) demo d)
  (DemoView d) `apply` DemoUpdated did demo = DemoView (M.adjust (const demo) did d)
  (DemoView d) `apply` DemoDeleted did      = DemoView (M.delete did d)


instance ToJSON (Event DemoView)
instance FromJSON (Event DemoView)

validate :: (a -> Bool) -> a -> s -> e -> Either e s
validate f a success failure | f a = return success
                             | otherwise = Left failure

-- | Get next numeric identifier for a Map.
nextId :: (Num a, Eq b, Eq a) => M.Map a b -> a
nextId v = if v == def then
             1
           else
             ((+1) . fst . M.findMax) v
