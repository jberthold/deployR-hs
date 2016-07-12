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

-- | overall API. Everything starts by "r"
type DeployRAPI =
  "r" :>
  (      DeployRUserAPI
    :<|> DeployRProjectAPI
    :<|> DeployRRepoAPI
    -- :<|> DeployRJobAPI :<|> DeployREventAPI -- irrelevant for our use case
  )

-- | User login, logout (Info and options not implemented)
type DeployRUserAPI =
    "user" :>
    (      "login" :> ReqBody '[FormUrlEncoded] LoginData
                   :> Post '[JSON] (DRResponse DRUser)
      :<|> "logout" :> ReqBody '[FormUrlEncoded] LogoutData
                    :> Post '[JSON] (DRResponse ())
    )

------------------------------------------------------------
-- | Project API: execution in context, project files, workspace (R
-- environment), project management (TODO not implemented)
type DeployRProjectAPI =
    "project" :>
    (      DeployRExecAPI -- execute in project (differs from "script" in repo
      :<|> DeployRPDirAPI -- project dir API, differs from Repo Dir API
      :<|> DeployRWorkspaceAPI -- specific to projects
      -- :<|> DeployROtherAPI -- creation, access, closing (sin-bin approach)
    )

-- | execution of code in project context
type DeployRExecAPI =
    "execute" :>
    ( "code" :> ReqBody '[FormUrlEncoded] ExecCode
             :> Post '[JSON] (DRResponse ExecResult)
      :<|> "script" :> ReqBody '[FormUrlEncoded] ExecScript
              :> Post '[JSON] (DRResponse ExecResult)
      :<|> "flush" -- :> ReqBody missing
              :> Post '[JSON] (DRResponse ()) -- TODO wrong response type
      -- and a few more, concerned with exec. history, not too relevant for us
    )

-- | Directory of a project
type DeployRPDirAPI =
    "directory" :>
    (      "list" -- :> Capture something missing
             :> Get '[JSON] (DRResponse [ProjectFile])
      :<|> "upload" -- :> ReqBody something missing
             :> Post '[JSON] (DRResponse ProjectFile)
      -- and a lot more...
    )

-- | Workspace of projects (R environment)
type DeployRWorkspaceAPI =
    "workspace" :>
    (       "list" -- :> Capture something missing
              :> Get '[JSON] (DRResponse ()) -- TODO wrong response type
     -- and a lot more...
    )

------------------------------------------------------------
-- File Repository API: directories (1 level), files, script execution
type DeployRRepoAPI =
    "repository" :>
    (      DeployRDirAPI    -- repo dir API
      :<|> DeployRFileAPI   -- repo file API
      :<|> DeployRScriptAPI -- script listing and execution
--    :<|> DeployRRepoMoreStuff -- all that we could not fit
            )

-- | Directories in projects and repository
type DeployRDirAPI = 
    "directory" :>
    (      "list" -- :> Capture something missing
              :> Get '[JSON] (DRResponse ()) -- TODO wrong return type
      :<|> "upload" -- :> ReqBody something missing
              :> Post '[JSON] (DRResponse RepoFile)
      -- and a lot more...
    )

-- | Files in the repository
type DeployRFileAPI = 
    "file" :>
    (      "list" -- :> Capture something missing -}
              :> Get '[JSON] (DRResponse [RepoFile])
      :<|> "upload" {- :> ReqBody something missing -}
              :> Post '[JSON] (DRResponse RepoFile)
      -- and a lot more...
    )

-- | execution and query of scripts that reside in the repository.
-- Not corresponding to execution of scripts in projects (although
-- largely similar)
type DeployRScriptAPI =
    "script" :>
    (      "list" {- :> QueryParam missing -}
              :> Get '[JSON] [RepoScript]
      :<|> "execute" {- :> ReqBody missing -}
              :> Post '[JSON] (DRResponse ())
      -- and two more ...
    )

------------------------------------------------------------
-- testing
deployRAPI = client (Proxy :: Proxy DeployRAPI) -- incomplete!
