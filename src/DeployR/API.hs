{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DeployR.API where
-- this will be split into several files, just for notes right now

import Servant.API
import Servant.Client

-- picked up from servant tutorial code. TODO reduce if possible
import Data.Proxy
import GHC.Generics

import Data.Text(Text)
import qualified Data.Text as T
import Data.List(isPrefixOf)

import DeployR.Types -- Data types for the API

----------------------------
-- DeployR API spec, grouped

-- | overall API. Everything starts by "r"
type DeployRAPI =
  "deployr" :> "r" :>
  (      DeployRUserAPI
    :<|> DeployRProjectAPI
    :<|> DeployRRepoAPI
    -- :<|> DeployRJobAPI :<|> DeployREventAPI -- irrelevant for our use case
  )

------------------------------------------------------------
-- pattern-matching out the sub-APIs, following the type structure
userAPI :<|> projectAPI :<|> repoAPI  = client (Proxy :: Proxy DeployRAPI)

-- | User login, logout (Info and options not implemented)
type DeployRUserAPI =
    "user" :>
    (      "login" :> ReqBody '[FormUrlEncoded] LoginData
                   :> Post '[JSON] (DRResponse DRUser)
      :<|> "logout" :> ReqBody '[FormUrlEncoded] LogoutData
                   :> Header "application:JSESSIONID" Text
                   :> Post '[JSON] (DRResponse ())
    )

-- pattern-matching out the client functions, following the type structure
login :<|> logout = userAPI

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
-- sub-APIs
execAPI :<|> pDirAPI :<|> wspaceAPI = projectAPI

-- | execution of code in project context
type DeployRExecAPI =
    "execute" :>
    ( "code" :> ReqBody '[FormUrlEncoded] ExecCode
             :> Header "application:JSESSIONID" Text
             :> Post '[JSON] (DRResponse ExecResult)
      :<|> "script" :> ReqBody '[FormUrlEncoded] ExecScript
              :> Header "application:JSESSIONID" Text
              :> Post '[JSON] (DRResponse ExecResult)
      :<|> "flush" :> ReqBody '[FormUrlEncoded] RqProject
             :> Header "application:JSESSIONID" Text
             :> Post '[JSON] (DRResponse ()) -- TODO wrong response type
      -- and a few more, concerned with exec. history, not too relevant for us
    )
              -- something is strange above, confuses Emacs haskell-mode...

-- sub-APIs
execCode :<|> execScript :<|> execFlush = execAPI

-- | Directory of a project
type DeployRPDirAPI =
    "directory" :>
    (      "list" :> QueryParam "format" Format -- not really needed
             :> QueryParam "project" Text
             :> Header "application:JSESSIONID" Text
             :> Get '[JSON] (DRResponse [ProjectFile])
      :<|> "upload" -- :> ReqBody something missing
             :> Header "application:JSESSIONID" Text
             :> Post '[JSON] (DRResponse ProjectFile)
      -- and a lot more...
    )
-- client functions
pDirList :<|> pDirUpload -- :<|> and more
  = pDirAPI

-- | Workspace of projects (R environment)
type DeployRWorkspaceAPI =
    "workspace" :>
    (       "list" -- :> Capture something missing
              :> Header "application:JSESSIONID" Text
              :> Get '[JSON] (DRResponse ()) -- TODO wrong response type
     -- and a lot more...
    )
-- client functions
wspaceList -- :<|> and more
  = wspaceAPI

------------------------------------------------------------
-- File Repository API: directories (1 level), files, script execution
type DeployRRepoAPI =
    "repository" :>
    (      DeployRDirAPI    -- repo dir API
      :<|> DeployRFileAPI   -- repo file API
      :<|> DeployRScriptAPI -- script listing and execution
--    :<|> DeployRRepoMoreStuff -- all that we could not fit
            )
-- sub-APIs
rDirAPI :<|> rFileAPI  :<|> rScriptAPI -- :<|> and more
  = repoAPI

-- | Directories in projects and repository
type DeployRDirAPI = 
    "directory" :>
    (      "list" -- :> Capture something missing
              :> Header "application:JSESSIONID" Text
              :> Get '[JSON] (DRResponse ()) -- TODO wrong return type
      :<|> "create" -- :> ReqBody something missing
             :> Header "application:JSESSIONID" Text
             :> Post '[JSON] (DRResponse ()) -- TODO wrong return type
      -- and a lot more...
    )
-- client functions
rDirList :<|> rDirCreate -- :<|> and more
  = rDirAPI

-- | Files in the repository
type DeployRFileAPI = 
    "file" :>
    (      "list" -- :> Capture something missing -}
              :> Header "application:JSESSIONID" Text
              :> Get '[JSON] (DRResponse [RepoFile])
      :<|> "upload" {- :> ReqBody something missing -}
              :> Header "application:JSESSIONID" Text
              :> Post '[JSON] (DRResponse RepoFile)
      -- and a lot more...
    )
-- client functions
rFileList :<|> rFileUpload -- :<|> and more
  = rFileAPI

-- | execution and query of scripts that reside in the repository.
-- Not corresponding to execution of scripts in projects (although
-- largely similar)
type DeployRScriptAPI =
    "script" :>
    (      "list" {- :> QueryParam missing -}
              :> Header "application:JSESSIONID" Text
              :> Get '[JSON] (DRResponse [RepoScript])
      :<|> "execute" {- :> ReqBody missing -}
              :> Header "application:JSESSIONID" Text
              :> Post '[JSON] (DRResponse ())
      -- and two more ...
    )
-- client functions
rScriptList :<|> rScriptExec -- :<|> and more
  = rScriptAPI
