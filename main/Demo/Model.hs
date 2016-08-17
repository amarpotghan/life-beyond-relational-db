{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Demo.Model where

import           Capital.EventSourced

import           Data.Aeson           as A
import           Data.Default
import           Data.Map             as M
import           Data.Text.Lazy       as L
import           GHC.Generics

type DemoId = Int

data Demo
  = Demo {
      demoId          :: DemoId
    , demoName        :: String
    , demoDescription :: L.Text
    }
  deriving (Show, Generic, Eq)

instance ToJSON Demo
instance FromJSON Demo

data DemoView
  = DemoView {
    confirmedDemos :: M.Map DemoId Demo
    }
  deriving (Show, Generic)

instance BusinessModel DemoView where

  data Command DemoView
    = AddDemo Demo
    | UpdateDemo DemoId Demo
    | DeleteDemo DemoId
    deriving (Show, Generic)

  data Event DemoView
    = DemoAdded Demo
    | DemoUpdated DemoId Demo
    | DemoDeleted DemoId
    deriving (Show, Generic)

  data Error DemoView
    = DemoNotFound DemoId
    | InvalidDemo
    deriving (Show, Generic)

  init = DemoView M.empty

  (DemoView d) `act` AddDemo demo =
    let did = nextId d
    in pure $ DemoAdded demo { demoId = did }
  (DemoView d) `act` UpdateDemo did newDemo =
    if M.member did d
    then Right (DemoUpdated did newDemo)
    else Left (DemoNotFound did)
  (DemoView d) `act` DeleteDemo did =
    if (M.member did d)
    then Right (DemoDeleted did)
    else Left (DemoNotFound did)
  (DemoView d) `apply` DemoAdded demo = DemoView (M.insert (demoId demo) demo d)
  (DemoView d) `apply` DemoUpdated did demo  = DemoView (M.adjust (const demo) did d)
  (DemoView d) `apply` DemoDeleted did = DemoView (M.delete did d)

instance ToJSON (Event DemoView)
instance FromJSON (Event DemoView)

-- | Get next numeric identifier for a Map.
nextId :: (Num a, Eq b, Eq a, Enum a) => M.Map a b -> a
nextId v =
  if M.null v
  then 1
  else (succ . fst . M.findMax) v
