module Auth0.Identity
  ( Identity (..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson as AE
import Data.Text (Text)

data Identity = Identity
  { userID     :: Text
  , provider   :: Text -- TODO: sum type
  , connection :: Text -- TODO: sum type
  , isSocial   :: Bool
  } deriving (Show, Eq)

instance ToJSON Identity where
  toJSON Identity{..} =
    AE.object [ "user_id"    .= userID
              , "provider"   .= provider
              , "connection" .= connection
              , "is_social"  .= isSocial
              ]

instance FromJSON Identity where
  parseJSON (AE.Object v) =
    Identity
      <$> v .: "user_id"
      <*> v .: "provider"
      <*> v .: "connection"
      <*> v .: "isSocial"
  parseJSON _ = mzero
