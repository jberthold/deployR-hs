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

