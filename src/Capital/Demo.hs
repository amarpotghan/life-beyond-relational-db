{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module Capital.Demo
       (demoService
       ) where

import           Capital.Demo.Model
import           Capital.Demo.Service
import           Capital.Demo.State

-- | Start server with given configuration
--
-- Returns the threadIds for Server, Logging, Driver and Storage threads
demoService :: IO ()
demoService = return ()






