module Auth0.Access
  ( AccessCode
  , AccessToken (..)
  , Config (..)
  , exchangeAccessCode
  , module JWT
  ) where

import Auth0.Config (Config (..))
import Control.Lens ((^.))
import Control.Monad (mzero)
import Control.Monad.Except (ExceptT, liftIO, throwError)
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson as AE
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics
import JWT
import Network.Wreq

type AccessCode = Text

data AccessToken = AccessToken
  { getToken     :: Text
  , getIdToken   :: Text
  , getTokenType :: Text -- TODO: sum type
  } deriving (Show, Eq, Generic)

instance ToJSON AccessToken where
  toJSON (AccessToken tok tokID tokType) =
    AE.object [ "access_token" .= tok
              , "id_token"     .= tokID
              , "token_type"   .= tokType
              ]

instance FromJSON AccessToken where
  parseJSON (AE.Object v) =
    AccessToken
      <$> v .: "access_token"
      <*> v .: "id_token"
      <*> v .: "token_type"
  parseJSON _ = mzero

-- TODO: replace Text with a data type
-- TODO: move auth0 domain to configuration
exchangeAccessCode :: AccessCode -> Config -> ExceptT Text IO AccessToken
exchangeAccessCode code Config{..} = do
  resp <- liftIO $ post (unpack $ getBasePath <> "/oauth/token") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err) -> throwError $ pack $ show err
    (Right tok) -> return tok
  where
    requestParams =
      [ "client_id"     := getClientID
      , "client_secret" := getClientSecret
      , "redirect_uri"  := getRedirectURI
      , "grant_type"    := getGrantType
      , "code"          := code
      ]
