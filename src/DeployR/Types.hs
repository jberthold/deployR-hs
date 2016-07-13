{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}

module DeployR.Types where

import Data.Aeson.Types
import GHC.Generics

import Data.Text(Text)
import qualified Data.Text as T
import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(isPrefixOf)
import Data.Maybe(catMaybes, maybeToList)

import Servant.API -- for instances

import DeployR.RObject

---------------------------------
-- Data types for the deployR API

-- | generic response type for DeployR.
-- The responses are largely the same, but may contain additional
-- fields in particular cases. Modelled by a type argument.
data DRResponse a =
  DRSuccess { -- if success is "true", read result payload and cookie
      drCall    :: Text
    , drCookie  :: Text
    , drExtra   :: a -- Extra data, often more than one thing
      -- This extra field will probably end up ugly for *JSON instances.
      -- The other option is to repeat all fields over and over...
    }
  | DRError {  -- if success is "false": error, retrieve message and code
      drError   :: Text
    , drErrCode :: Int
    , drCookie  :: Text
    }
  deriving (Eq, Show, Read, Generic)

-- | helper to dive into object hierarchies
-- I thought withObject would do this (one level), but found out it does not.
-- The Value is verified to be an Object, then the named fields from
-- it are selected in order, all expected to be nested object. The extractA
-- function is applied to the innermost object to yield the desired result a.
inPath :: [Text] -> (Object -> Parser a) -> Value -> Parser a
inPath fs extractA = withObject ("path " ++ T.unpack (T.intercalate "." fs))
                                (foldr descend extractA fs)
  where descend :: Text -> (Object -> Parser a) -> Object -> Parser a
        descend n f o = o .: n >>= withObject (T.unpack $ T.unwords ["object",n]) f
        -- meaning: "parse an object called n inside o, then do f with it"
{-
with an Object -> Parser a function:

   (Object -> Parser a) -> Text -> (Object -> Parser a)
   \ extractO              name ->
         withObject (printf "object with %s" name)
                    (\o -> o .: name >>= extract)

-}

-- | The JSON parser for DRResponse a uses a parseJSONPayload parser for a.
instance (FromJSONPayload a) => FromJSON (DRResponse a) where
  parseJSON = inPath ["deployr", "response"] parseResponse
      where parseResponse re = do
              ok    <- re .: "success"
              if ok then DRSuccess <$> re .: "call"
                                   <*> re .: "httpcookie"
                                   <*> parseJSONPayload re
                else DRError <$> re .: "error"
                             <*> re .: "errorCode"
                             <*> re .: "httpcookie"

-- | a special type class for the payload in a DeployR response.  If this
-- was simply using FromJSON, one would need to create bogus parsers that
-- could not be used outside the context of the parseJSON :: Value ->
-- DRResponse instance. For instance, the payload might consist of multiple
-- members of the response object, which we can model as tuples here.
class FromJSONPayload a where
  parseJSONPayload :: Object -> Parser a -- used inside the DRResponse parser

-- Response payload data

-- no payload in response
instance FromJSONPayload () where
  parseJSONPayload _ = return ()

-- | response to login
data DRUser = DRUser {
      username    :: Text
    , displayname :: Text
    , cookie      :: Text
    , permissions :: DRPermissions
    -- this is the "user" structure, "limits" struture has been omitted
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON DRUser
instance FromJSONPayload DRUser where
  parseJSONPayload r = r .: "user" >>= parseJSON

data DRPermissions = DRPermissions {
      scriptManager  :: Bool
    , powerUser      :: Bool
    , packageManager :: Bool
    , administrator  :: Bool
    , basicUser      :: Bool
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON DRPermissions

-- | Files in the repository, represented in responses
data RepoFile = RepoFile {
      filename  :: FilePath
    , directory :: FilePath
    , descr     :: Text
    , length    :: Int
    , authors   :: [Text]
    , shared    :: Bool
    , restricted :: Bool
    , url       :: Text -- TODO use URL type
    -- , type      :: Text -- application/octet-stream or text/plain, mostly
    -- plus some version and access stuff which was not modelled
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON RepoFile
instance FromJSONPayload RepoFile where
  parseJSONPayload r = r .: "repository" >>=  (.: "file") >>= parseJSON

instance FromJSONPayload [RepoFile] where
  parseJSONPayload r = r .: "repository" >>=  (.: "files") >>= parseJSON

-- | "script" alias for repository files, to parse payload in responses
newtype RepoScript = Script RepoFile
                  -- deriving (Eq, Show, Read, Generic)

instance FromJSON RepoScript where
  parseJSON o = Script <$> parseJSON o
instance FromJSONPayload [RepoScript] where
  parseJSONPayload r = r .: "repository" >>=
                       (.: "scripts") >>= \fs ->
                       map Script <$> (parseJSON fs:: Parser [RepoFile])

-- | Files in a project directory
data ProjectFile = ProjectFile {
      filename  :: FilePath
    , descr     :: Maybe Text
    , length    :: Int
    , url       :: Text -- TODO use URL type
    -- , type      :: Text -- application/octet-stream or text/plain, mostly
    -- ... more fields: lastmodified, category, not implemented
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON ProjectFile
instance FromJSONPayload ProjectFile where
  parseJSONPayload r = r .: "directory" >>=  (.: "file") >>= parseJSON

-- project directory listing:
instance FromJSONPayload [ProjectFile] where
  parseJSONPayload r = r .: "directory" >>=  (.: "files") >>= parseJSON

-- | Result of executing code (literal or a script)
data ExecResult = ExecResult {
      interrupted :: Bool
    , project     :: DRProject
    , objects     :: [RObject]
    , execution   :: DRExecution
      -- , files :: [RepoFile]
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON ExecResult
instance FromJSONPayload ExecResult where
  parseJSONPayload r = do
      interrupted <- r .: "interrupted"
      project     <- r .: "project" >>= parseJSON
      execution   <- r .: "execution" >>= parseJSON
      objects     <- r .: "workspace" >>= (.: "objects") >>= parseJSON
      return ExecResult{..}

data DRProject = DRProject {
      project :: Text
    , name    :: Text
    , descr   :: Maybe Text
      -- , a few more... 
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON DRProject

data DRExecution = DRExecution {
      console :: Text
    , code    :: Text
--     , and more things
    }
        deriving (Eq, Show, Read, Generic)

instance FromJSON DRExecution

--------------------------------------------------
-- input data (will be used in ReqBody FormUrlEncoded)

------------------------------------------------------------
-- helpers

-- | include optional fields if present, in ToFormUrlEncoded. Field content is
-- transformed to text
optional :: (value -> Text) -> (Text, Maybe value) -> Maybe (Text, Text)
optional convert (name, Nothing) = Nothing
optional convert (name, Just v)  = Just (name, convert v)
-- TODO could use a type class for value -> Text

-- | optional text fields (passed through directly if present)
optionalText = optional id

optionalBool = optional (\b -> if b then "true" else "false")

-- a little CPP hack to avoid repeating myself all the time
-- #define FIELD( S ) ( #S , S)
-- alas, GHC does not support stringification...

----------------------------------------
-- form data types, replicating required deployR input format exactly

-- tired to write this over and over again, we always use json anyway
data Format = FormatJSON deriving (Eq, Generic) -- will this work?
instance Show Format where show _ = "json"
instance Read Format where readsPrec _ input
                               | "json" `isPrefixOf` input
                                   = [(FormatJSON, drop 4 input)]
                               | otherwise = []
instance ToJSON Format where

-- | even simpler: include this in every POST and request
formatEncoded :: (Text, Text)
formatEncoded = ("format", "json")

-- | user login, with password
data LoginData = LoginData {
      format   :: Format -- requires ghc-8 (duplicate record field names)
    , username :: Text 
    , password :: Text
    , disableautosave :: Maybe Bool
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON LoginData
instance ToFormUrlEncoded LoginData where
  toFormUrlEncoded LoginData{..} =
    formatEncoded:
    [ ("username", username)
    , ("password", password)
    ]
    ++ maybeToList (optionalBool ("disableautosave", disableautosave))

-- | user login, with password
data LogoutData = LogoutData {
      format   :: Format       -- "json"
    , usercookie :: Maybe Text -- not used
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON LogoutData
instance ToFormUrlEncoded LogoutData where
  toFormUrlEncoded LogoutData{..} =
    formatEncoded:
    maybeToList (optionalText ("usercookie", usercookie))

-- | executing code in a project context
-- we have omitted a number of parameters here
data ExecCode = ExecCode {
      format :: Format
    , project :: Text
    , code :: Text
      -- pre-execution parameters
    , inputs :: Maybe Text
      -- on-execution parameters
    , phantom :: Maybe Bool
    , echooff :: Maybe Bool
    , consoleoff :: Maybe Bool
    , artifactsoff :: Maybe Bool
      -- post-execution parameters
    , robjects :: Maybe [Text]
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON ExecCode
instance ToFormUrlEncoded ExecCode where
  toFormUrlEncoded ExecCode{..} =
    formatEncoded :
    concat
    [[ ("project", project)
     , ("code", code)
     ]
    , maybeToList (optional (T.intercalate ",") ("robjects", robjects))
    , catMaybes $ map optionalBool
      [ ("phantom", phantom)
      , ("echooff", echooff)
      , ("consoleoff", consoleoff)
      , ("artifactsoff", artifactsoff)
      ]
    ]

-- | executing a script from the repository in a project context
-- The API call would allow multiple scripts, or even external files, to be
-- executed in a chain. We limit the API to one script from the repository,
-- and omit a number of parameters here (as in ExecCode)
data ExecScript = ExecScript {
      format :: Format
    , project :: Text
    , filename :: Text
    , author :: Text
    , directory :: Maybe Text
      -- pre-execution parameters
    , inputs :: Maybe Text
      -- on-execution parameters
    , phantom :: Maybe Bool
    , echooff :: Maybe Bool
    , consoleoff :: Maybe Bool
    , artifactsoff :: Maybe Bool
      -- post-execution parameters
    , robjects :: Maybe [Text]
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON ExecScript
instance ToFormUrlEncoded ExecScript where
  toFormUrlEncoded ExecScript{..} =
    formatEncoded :
    concat
    [[ ("project", project)
     , ("filename", filename)
     , ("author", author) ]
    , maybeToList (optionalText ("inputs", inputs))
    , maybeToList (optionalText ("directory", directory))
    , catMaybes $ map optionalBool [ ("phantom", phantom)
                                   , ("echooff", echooff)
                                   , ("consoleoff", consoleoff)
                                   , ("artifactsoff", artifactsoff)
                                   ]
    , maybeToList (optional (T.intercalate ",") ("robjects", robjects))
    ]
