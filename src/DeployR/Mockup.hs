{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- a simple mockup server for the DeployR API
-- module DeployR.Mockup where

module Main where

import Data.Proxy
import Servant
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Data.Aeson
import Data.Maybe

import Data.Text(Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import DeployR.API
import DeployR.Types
import DeployR.RObject

-- run the server using Wai and Warp
main :: IO ()
main = do args <- getArgs
          let port = if null args then 8000 else read (head args)
          putStrLn $ "DeployR Mock-up Server on port " ++ show port
          putStrLn "Press ^C to exit"
          run port mockserver

mockserver :: Application
mockserver = serve (Proxy :: Proxy DeployRAPI) mock

-- the server (lots of writing to do...follows the API structure)
mock :: Server DeployRAPI
mock =
  -- user API
  (failWithLog1 "login" :<|> failWithLog1 "logout")
    -- project API
  :<|> ((failWithLog1 "exec.code" :<|> failWithLog1 "exec.script" :<|> failWithLog0 "exec.flush" ) -- exec
        :<|> (failWithLog0 "pdir.list" :<|> failWithLog0 "pdir.upload") -- pdir
        :<|> failWithLog0 "wspace.list"
       ) -- wspace
  :<|> ((failWithLog0 "repo.dir.list" :<|> failWithLog0 "repo.dir.create") -- rdir
        :<|> (failWithLog0 "repo.file.list" :<|> failWithLog0 "repo.file.upload") -- rfile
        :<|> (failWithLog0 "repo.script.list" :<|> failWithLog0 "repo.script.exec") -- rscript
       )

-- needs some ToJSON instances which we do not use
-- (ATTENTION, these instances do not work properly!)
instance ToJSON (DRResponse a) where
  toJSON DRError{..}   =
    object
    [ "deployr" .= object
      [ "response" .= object
        ([ "success" .= False
         , "errorCode" .= drErrCode
         , "error"     .= drError
         ] ++ maybeToList ("httpcookie" .=? drCookie))
      ]
    ]
  toJSON DRSuccess{..} = toJSON DRError{ drCookie  = drCookie
                                       , drErrCode = 666
                                       , drError   = "Returning a success, why?" }
(.=?) field  = fmap (\mx -> field .= mx)
 

instance ToJSON RepoFile
instance ToJSON DRUser
instance ToJSON DRPermissions
instance ToJSON ExecResult
instance ToJSON DRProject
instance ToJSON RObject
instance ToJSON DRExecution
instance ToJSON ProjectFile
instance ToJSON RepoScript


------------------------------------------------------------
-- FromFormUrlEncoded instances.
--- These do the correct job (we want to see the requests)

-- helper for FromFormUrlEncoded
from :: [(Text, Text)] -> Text -> (Text -> a) -> Either String a
from pairs txt convert =
  maybe (Left $ T.unpack txt ++ " not found") (Right . convert) $ lookup txt pairs

textFrom pairs txt = from pairs txt id
readFrom pairs txt = from pairs txt (read . T.unpack)

instance FromFormUrlEncoded LoginData where
  fromFormUrlEncoded pairs = do
    format     <- readFrom pairs "format"
    username   <- textFrom pairs "username"
    password   <- textFrom pairs "password"
    let disableautosave = fmap (=="true") $ lookup "disableautosave" pairs 
    return LoginData{..}

instance FromFormUrlEncoded LogoutData where
  fromFormUrlEncoded pairs = do
    format     <- readFrom pairs "format"
    let usercookie = lookup "usercookie" pairs 
    return LogoutData{..}

instance FromFormUrlEncoded ExecCode where
  fromFormUrlEncoded pairs = do
    format     <- readFrom pairs "format"
    project    <- textFrom pairs "project"
    code       <- textFrom pairs "code"
    let inputs    = lookup "inputs" pairs
        phantom   = fmap (=="true") $ lookup "phantom" pairs
        echooff   = fmap (=="true") $ lookup "echooff" pairs
        consoleoff= fmap (=="true") $ lookup "consoleoff" pairs
        artifactsoff = fmap (=="true") $ lookup "artifactsoff" pairs
        robjects  = fmap (T.splitOn ",") $ lookup "robjects" pairs
    return ExecCode{..}

instance FromFormUrlEncoded ExecScript where
  fromFormUrlEncoded pairs = do
    format     <- readFrom pairs "format"
    project    <- textFrom pairs "project"
    filename   <- textFrom pairs "filename"
    author     <- textFrom pairs "author"
    let directory = lookup "directory" pairs
        inputs    = lookup "inputs" pairs
        phantom   = fmap (=="true") $ lookup "phantom" pairs
        echooff   = fmap (=="true") $ lookup "echooff" pairs
        consoleoff= fmap (=="true") $ lookup "consoleoff" pairs
        artifactsoff = fmap (=="true") $ lookup "artifactsoff" pairs
        robjects  = fmap (T.splitOn ",") $ lookup "robjects" pairs
    return ExecScript{..}


-- dummy request handlers
failWithLog0 :: Text -> Handler (DRResponse b)
failWithLog0 req = do liftIO (T.putStr msg)
                      return failed
  where failed = DRError { drCookie  = Nothing
                         , drErrCode = 666
                         , drError   =  msg }
        msg    = T.intercalate "\t" [ "MOCK SERVER:\tRequest"
                                    , req, "(no body)" ]

-- universal log/fail handler
failWithLog1 :: (Show a) => Text -> a -> Handler (DRResponse b)
failWithLog1 req arg1 = do liftIO (T.putStrLn msg)
                           return failed
  where failed = DRError { drCookie  = Nothing
                         , drErrCode = 666
                         , drError   = msg }
        msg    = T.intercalate "\t" [ "MOCK SERVER:\tRequest"
                                    , req, T.pack $ show arg1 ]

-- fail handler which accepts and sets a cookie header
failCookie :: (Show a) => Text -> Maybe Text -> a -> Handler (DRResponse b)
failCookie req cookie x = failWithLog1 req x >>= \resp ->
                          return $ resp{ drCookie = cookie }
