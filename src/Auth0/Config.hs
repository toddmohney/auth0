module Auth0.Config
  ( Config (..)
  ) where

import Data.Text (Text)

data Config = Config
  { getClientID     :: Text
  , getClientSecret :: Text
  , getRedirectURI  :: Text
  , getGrantType    :: Text -- TODO: sum type
  , getBasePath     :: Text
  , getAPIAccessToken :: Text
  } deriving (Show, Eq)
