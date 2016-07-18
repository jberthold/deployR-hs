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
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Servant.API
import Servant.Client

-- Client requests run in the ClientM monad (once supplied with their payload,
-- a Manager and a BaseUrl).
-- simple http URl with flexible host, port 8000
getBaseUrl :: IO BaseUrl
getBaseUrl = do putStr "Enter DeployR host name: "
                baseUrlHost <- getLine
                return BaseUrl{..}
  where baseUrlPort   = 8000
        baseUrlScheme = Http
        baseUrlPath   = ""

getMgr :: IO Manager
getMgr = newManager defaultManagerSettings


itsMe :: LoginData
itsMe = LoginData FormatJSON "admin" "secret" Nothing


loginTest :: IO ()
loginTest = do
  mgr  <- getMgr
  base <- getBaseUrl
  result <- runExceptT (login itsMe mgr base)
  print result

-- | 
withCookieRun :: (FromJSONPayload b, Show b) =>
                 BaseUrl ->
                 Text ->
                 (a -> Maybe Text -> Manager -> BaseUrl -> ClientM (DRResponse b)) ->
                 a ->
                 IO ()
withCookieRun base cookie req reqData = do
  mgr  <- getMgr
  result <- runExceptT (req reqData (Just $ T.concat ["JSESSIONID=",cookie]) mgr base)
  print result

-- we would like an "inSession" combinator to run a ClientM with an
-- authentication header on each action (the httpcookie)
