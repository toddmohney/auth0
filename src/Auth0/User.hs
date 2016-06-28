module Auth0.User
  ( User (..)
  , getUser
  ) where

import Auth0.Access (AccessToken (..))
import Auth0.Config (Config (..))
import Auth0.Identity (Identity (..))
import Control.Lens ((^.), (.~), (&))
import Control.Monad (mzero)
import Control.Monad.Except (ExceptT, liftIO, throwError)
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson as AE
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Network.Wreq

data User = User
  { userID        :: Text
  , email         :: Text
  , emailVerified :: Bool
  , picture       :: Text -- TODO: convert to url
  , name          :: Text
  , nickname      :: Text
  , identities    :: [Identity]
  } deriving (Show, Eq)

instance ToJSON User where
  toJSON User{..} =
    AE.object [ "user_id"        .= userID
              , "email"          .= email
              , "email_verified" .= emailVerified
              , "picture"        .= picture
              , "name"           .= name
              , "nickname"       .= nickname
              , "identities"     .= identities
              ]

instance FromJSON User where
  parseJSON (AE.Object v) =
    User
      <$> v .: "user_id"
      <*> v .: "email"
      <*> v .: "email_verified"
      <*> v .: "picture"
      <*> v .: "name"
      <*> v .: "nickname"
      <*> v .: "identities"
  parseJSON _ = mzero

-- TODO: handle errors
-- TODO: move auth0 domain to configuration
getUser :: AccessToken -> Config -> ExceptT Text IO User
getUser AccessToken{..} Config{..} = do
  resp <- liftIO $ getWith opts (unpack $ getBasePath <> "/userinfo")
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err) -> throwError $ pack $ show err
    (Right tok) -> return tok
  where
    opts = defaults & param "access_token" .~ [getToken]
