{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Api.TextLocal
  (Credential
  ,mkCredentials
  ,SMSSettings
  ,defaultSMSSettings
  ,setManager
  ,setDestinationNumber
  ,setMessage
  ,setAuth
  ,runSettings
  ,testIO -- needs to be deleted
  ,sendSMS)
  where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.UnixTime (UnixTime)
import Network.HTTP.Simple -- (setRequestManager, httpJSONEither)
import Network.HTTP.Client (Manager, parseRequest, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import Data.Monoid ((<>))
import Network.Api.Types

-- |
-- Credential for making request to textLocal server.
data Credential
    = ApiKey ByteString
    | UserHash ByteString -- ^ Email address
               ByteString -- ^ Secure hash that is found within the messenger.

baseUrl :: String
baseUrl = "https://api.textlocal.in/"

-- Create 'Credential' for textLocal.
mkCredentials
    :: ByteString -- ^ api key
    -> Credential
mkCredentials apiKey = ApiKey apiKey

formCred (ApiKey apikey) = [partBS "apiKey" apikey]
formCred (UserHash user hash) = [partBS "username" user, partBS "hash" hash]

data SMSSettings = SMSSettings
    { settingsSender :: ByteString -- ^ Sender name must be 6 alpha
                                   -- characters and should be
                                   -- pre-approved by Textlocal. In
                                   -- the absence of approved sender
                                   -- names, use the default 'TXTLCL'
    , settingsMessage :: ByteString -- ^ The message content. This
                                    -- parameter should be no longer
                                    -- than 766 characters. The
                                    -- message also must be URL
                                    -- Encoded to support symbols like
                                    -- &.
    , settingsAuth :: Credential
    , settingsNumber :: [ByteString]
    , settingsManager :: Maybe Manager
    , settingsGroupId :: Maybe Int
    , settingsScheduleTime :: Maybe UnixTime
    , settingsReceiptUrl :: Maybe ByteString
    , settingsCustom :: Maybe Text
    , settingsOptOut :: Maybe Bool
    , settingsValidity :: Maybe UnixTime
    , settingsUnicode :: Maybe Bool
    , settingsTest :: Maybe Bool
    }

-- | 'defaultSMSSettings' has the default settings, duh! The
-- 'settingsSender' has a value of "TXTLCL". The accessors
-- 'settingsMessage', 'settingsAuth', 'settingsNumber' contains a
-- value of bottom. They have to be initialized to a proper value
-- before sending SMS.
defaultSMSSettings =
    SMSSettings
    { settingsSender = "TXTLCL"
    , settingsMessage = error "defaultSMSSettings: message content not present"
    , settingsAuth = error "defaultSMSSettings: credential not present"
    , settingsNumber = error "defaultSMSSettings: no number present"
    , settingsManager = Nothing
    , settingsGroupId = Nothing
    , settingsScheduleTime = Nothing
    , settingsReceiptUrl = Nothing
    , settingsCustom = Nothing
    , settingsOptOut = Nothing
    , settingsValidity = Nothing
    , settingsUnicode = Nothing
    , settingsTest = Just False
    }

setMessage :: ByteString -> SMSSettings -> SMSSettings
setMessage msg def =
    def
    { settingsMessage = msg
    }

setAuth :: Credential -> SMSSettings -> SMSSettings
setAuth cred def =
    def
    { settingsAuth = cred
    }

setDestinationNumber :: [ByteString] -> SMSSettings -> SMSSettings
setDestinationNumber num def =
    def
    { settingsNumber = num
    }

-- | Use an existing manager instead of creating a new one
setManager
    :: Manager -> SMSSettings -> SMSSettings
setManager mgr def =
    def
    { settingsManager = Just mgr
    }

setTest :: Bool -> SMSSettings -> SMSSettings
setTest test set =
    set
    { settingsTest = Just test
    }

sendSMS
    :: ByteString -- ^ Text to send
    -> [ByteString] -- ^ Destination numbers
    -> Credential
    -> IO (Either JSONException TLResponse)
sendSMS msg num cred =
    runSettings
        SendSMS
        defaultSMSSettings
        { settingsAuth = cred
        , settingsNumber = num
        , settingsMessage = msg
        }

data Command =
    SendSMS
    deriving (Show,Eq,Ord)

testIO = runSettings SendSMS testSettings

runSettings :: Command -> SMSSettings -> IO (Either JSONException TLResponse)
runSettings cmd set = do
    let epoint = endpointUrl cmd
    req <- parseRequest epoint
    mgr <- getManager (settingsManager set)
    req' <-
        formDataBody
            ([ partBS "sender" (settingsSender set)
             , partBS "message" (settingsMessage set)
             , partBS "numbers" (B.intercalate "," $ settingsNumber set)
             , partBS "test" (boolByteString $ settingsTest set)] <>
             formCred (settingsAuth set))
            req
    let req'' = setRequestManager mgr req'
    (response :: Response (Either JSONException TLResponse)) <-
        httpJSONEither req''
    return $ getResponseBody response

boolByteString :: Maybe Bool -> ByteString
boolByteString Nothing = "false"
boolByteString (Just False) = "false"
boolByteString (Just True) = "true"

credentialByteString :: Credential -> ByteString
credentialByteString (ApiKey bytes) = bytes

getManager :: Maybe Manager -> IO Manager
getManager Nothing = newManager tlsManagerSettings
getManager (Just mgr) = return mgr

endpointUrl :: Command -> String
endpointUrl SendSMS = baseUrl <> "send/"