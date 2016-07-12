{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DeployR.Types where

import Data.Aeson
import GHC.Generics

import Data.Text(Text)
import qualified Data.Text as T
import Data.List(isPrefixOf)
import Data.Maybe(catMaybes)

import Servant.API -- for instances

---------------------------------
-- Data types for the deployR API

-- | generic response type for DeployR.
-- The responses are largely the same, but may contain additional
-- fields in particular cases. Modelled by a type argument.
data DRResponse a = DRResponse {
      drSuccess :: Bool
    , drCall    :: Text
    , drCookie  :: Maybe Text
    , drExtra   :: a -- Extra data, often more than one thing
      -- This extra field will probably end up ugly for *JSON instances.
      -- The other option is to repeat all fields over and over...
    }
    deriving (Eq, Show, Read, Generic)

instance (FromJSON a) => FromJSON (DRResponse a)
         -- TODO this will need a manual definition to parse away the
         -- "deployr"/"response" prefix and integrate the payload data
         -- correctly. Maybe create a type class for the latter? (tuples
         -- become different members in the response object)


-- Response payload data

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

data DRPermissions = DRPermissions {
      scriptManager  :: Bool
    , powerUser      :: Bool
    , packageManager :: Bool
    , administrator  :: Bool
    , basicUser      :: Bool
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON DRPermissions

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

instance FromJSON DRFile

--------------------------------------------------
-- input data (will be used in ReqBody FormUrlEncoded)

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
    ++ catMaybes
    [ optional (\b -> if b then "true" else "false")
      ("disableautosave", disableautosave)
    ]

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
    catMaybes (map optionalText
               [ ("usercookie", usercookie)
               ])

-- helpers

-- | include optional fields if present, in ToFormUrlEncoded. Field content is
-- transformed to text
optional :: (value -> Text) -> (Text, Maybe value) -> Maybe (Text, Text)
optional convert (name, Nothing) = Nothing
optional convert (name, Just v)  = Just (name, convert v)
-- TODO could use a type class for value -> Text

-- | optional text fields (passed through directly if present)
optionalText = optional id
