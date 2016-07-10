module Auth0.Identity
  ( Identity (..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson as AE
import Data.Text (Text, pack)

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

{- |
  Certain providers send a 'user_id' as an Int while others use a String
-}
instance FromJSON Identity where
  parseJSON = AE.withObject "identity" $ \v -> do
    uID    <- v .: "user_id" >>= parseID
    prov   <- v .: "provider"
    conn   <- v .: "connection"
    social <- v .: "isSocial"
    return $ Identity uID prov conn social
    where
      parseID (AE.String strId) = return strId
      parseID (AE.Number numId) = return . pack . show $ numId
