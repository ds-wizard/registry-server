module Api.Resource.ActionKey.ActionKeyJM where

import Control.Monad
import Data.Aeson

import Api.Resource.ActionKey.ActionKeyDTO

instance FromJSON ActionKeyDTO where
  parseJSON (Object o) = do
    _actionKeyDTOAType <- o .: "type"
    _actionKeyDTOEmail <- o .: "email"
    return ActionKeyDTO {..}
  parseJSON _ = mzero

instance ToJSON ActionKeyDTO where
  toJSON ActionKeyDTO {..} = object ["type" .= _actionKeyDTOAType, "email" .= _actionKeyDTOEmail]
