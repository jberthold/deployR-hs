{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DeployR where
-- this will be split into several files, just for notes right now

import Servant.API
import Servant.Client

-- picked up from servant tutorial code. TODO reduce if possible
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Data.Text(Text)
import qualified Data.Text as T
import Data.List(isPrefixOf)

import DeployR.Types -- Data types for the API

----------------------------
-- DeployR API spec, grouped

type DeployRAPI = DeployRUserAPI
    :<|> DeployRProjectAPI
    :<|> DeployRRepoAPI
    -- :<|> DeployRJobAPI :<|> DeployREventAPI -- irrelevant for our use case

type DeployRUserAPI =
    "r" :> "user" :>
    ( "login" :> ReqBody '[FormUrlEncoded] LoginData
              :> Post '[JSON] (DRResponse DRUser)
      :<|> "logout" :> ReqBody '[FormUrlEncoded] LogoutData
                    :> Post '[JSON] (DRResponse ())
    )

type DeployRProjectAPI =
    "r" :> "project" :>
            (      DeployRDirAPI  -- also without a project. Same?
              :<|> DeployRExecAPI -- not the same as "script" for repo
              :<|> DeployRWorkspaceAPI -- specific to projects
              -- :<|> DeployROtherAPI -- creation, access, closing
              --                      -- (sin-bin approach)
            )

type DeployRRepoAPI =
    "r" :> "repository" :>
            ( DeployRDirAPI 
              :<|> DeployRFileAPI
              :<|> DeployRScriptAPI
--              :<|> DeployRRepoMoreStuff -- all that we could not fit
            )

-- shared sub-APIs
-- | Directories in projects and repository
type DeployRDirAPI = 
    -- TODO misunderstanding, "list" is different for the repository!
    "directory" :> ( "list" {- :> Capture something missing -}
                       :> Get '[JSON] (DRResponse [DRFile])
                :<|> "upload" {- :> ReqBody something missing -}
                              :> Post '[JSON] (DRResponse DRFile)
                -- and a lot more...
              )

-- | Directories in projects and repository
type DeployRWorkspaceAPI = 
    "workspace" :> ( "list" {- :> Capture something missing -}
                            :> Get '[JSON] (DRResponse ())
                   -- TODO not really unit, define response type
                   -- and a lot more...
              )

-- | Files in the repository
type DeployRFileAPI = 
    "file" :> ( "list" {- :> Capture something missing -}
                       :> Get '[JSON] (DRResponse [DRFile])
                :<|> "upload" {- :> ReqBody something missing -}
                              :> Post '[JSON] (DRResponse DRFile)
                -- and a lot more...
              )

-- | execution and query of scripts that reside in the repository.
-- Not corresponding to execution of scripts in projects (although
-- largely similar)
type DeployRScriptAPI =
    "script" :> ( "list" {- :> QueryParam missing -}
                         :> Get '[JSON] [DRFile]
                  :<|> "execute" {- :> ReqBody missing -}
                                 :> Post '[JSON] (DRResponse ())
                -- and two more ... 
                )

-- | execution of code in project context
type DeployRExecAPI =
    "execute" :> ( "code" {- :> ReqBody missing -}
                          :> Post '[JSON] (DRResponse ())
                   :<|> "script" {- :> ReqBody missing -}
                                 :> Post '[JSON] (DRResponse ())
                   :<|> "flush" {- :> ReqBody missing -}
                                :> Post '[JSON] (DRResponse ())
                   -- TODO proper result type. Unit is wrong here.
                   -- and a few more, concerned with exec. history
                 )

------------------------------------------------------------
-- testing
worx    = client (Proxy :: Proxy DeployRScriptAPI) -- but is incomplete
-- doznwok = client (Proxy :: Proxy DeployRUserAPI)
