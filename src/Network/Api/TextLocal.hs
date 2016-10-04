{-# LANGUAGE OverloadedStrings #-}

module Network.Api.TextLocal
  (Crediential)
  where

import Data.Text (Text)

-- | Credential for making request to textLocal server.
data Crediential
    = ApiKey Text
    | UserHash Text -- ^ Email address
               Text -- ^ Secure hash that is found within the messenger.

baseUrl :: Text
baseUrl = "https://api.textlocal.in/"

-- Create 'Credential' for textLocal.
mkCredentials
    :: Text -- ^ api key
    -> Crediential
mkCredentials apiKey = ApiKey apiKey