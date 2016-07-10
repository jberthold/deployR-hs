{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

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
                          :> Post '[JSON] DRResponse
                   :<|> "script" {- :> ReqBody missing -}
                                 :> Post '[JSON] (DRResponse ())
                   :<|> "flush" {- :> ReqBody missing -}
                                :> Post '[JSON] (DRResponse ())
                   -- TODO proper result type. Unit is wrong here.
                   -- and a few more, concerned with exec. history
                 )

-- ... lots of writing to do...
--------------------------------
-- Data types for the API

-- | generic response type for DeployR.
-- The responses are largely the same, but may contain additional
-- fields in particular cases. Modelled by a type argument.
data DRResponse a = DRResponse {
      drSuccess :: Bool
    , drCall    :: Text
    , drCookie  :: Maybe Text
    , drExtra   :: a -- Extra data, often more than one thing
    }
    deriving (Eq, Show, Read, Generic)

-- This extra field will probably end up ugly for *JSON instances.
-- The other option is to repeat all fields over and over...


-- form data types, replicating required deployR input format exactly

-- | tired to write this over and over again, we always use json anyway
data Format = FormatJSON
            deriving (Eq, Generic) -- will this work?

instance Show Format where show _ = "json"
instance Read Format where readsPrec _ input
                               | "json" `isPrefixOf` input
                                   = [(FormatJSON, drop 4 input)]
                               | otherwise = []

-- | user login, with password
data LoginData = LoginData {
      format1   :: Format -- TODO use ghc-8! (duplicate record field names)
    , username :: Text 
    , password :: Text
    , disableautosave :: Maybe Bool
    }
    deriving (Eq, Show, Read, Generic)

-- | user login, with password
data LogoutData = LogoutData {
      format2   :: Format -- "json"
--    , usercookie :: Maybe Text -- unused
    }
    deriving (Eq, Show, Read, Generic)

-- | response to login
data DRUser = DRUser {
      username1 :: Text -- TODO use ghc-8
    , displayname :: Text
    , cookie :: Text
    , permissions :: DRPermissions
    -- this is the "user" structure, "limits" struture has been omitted
    }
    deriving (Eq, Show, Read, Generic)

data DRPermissions = DRPermissions {
      scriptManager :: Bool
    , powerUser :: Bool
    , packageManager :: Bool
    , administrator :: Bool
    , basicUser :: Bool
    }
    deriving (Eq, Show, Read, Generic)

data DRFile = DRFile {
      filename  :: FilePath
    , directory :: FilePath
    , descr     :: Text
    , length    :: Int
    , authors   :: [Text]
    , shared    :: Bool
    , restricted :: Bool
    -- , type      :: Text -- application/octet-stream or text/plain...
    , url       :: Text -- TODO use URL type
    -- plus some version and access stuff which was not modelled
    }
    deriving (Eq, Show, Read, Generic)
