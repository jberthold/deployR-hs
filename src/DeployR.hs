{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DeployR( module DeployR.API
              , module DeployR.Types
              ) where
-- this is an umbrella module

import DeployR.Types -- Data types for the API
import DeployR.API   -- the API itself, including client functions

-- picked up from servant tutorial code. TODO reduce if possible
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
-- import Data.Proxy
-- import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Servant.API
import Servant.Client

-- Client requests run in the ClientM monad (once supplied with their payload,
-- a Manager and a BaseUrl).
logMeIn :: Manager -> BaseUrl -> ClientM (DRResponse DRUser)
logMeIn = login itsMe

itsMe :: LoginData
itsMe = LoginData FormatJSON "admin" "secret" Nothing

-- we would like an "inSession" combinator to run a ClientM with an
-- authentication header on each action (the httpcookie)
