module Api.Resource.Organization.OrganizationJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.Common
import Api.Resource.Organization.OrganizationDTO

instance FromJSON OrganizationDTO where
  parseJSON (Object o) = do
    _organizationDTOOrganizationId <- o .: "organizationId"
    _organizationDTOName <- o .: "name"
    _organizationDTODescription <- o .: "description"
    _organizationDTOEmail <- o .: "email"
    _organizationDTOToken <- o .: "token"
    _organizationDTOActive <- o .: "active"
    _organizationDTOLogo <- o .: "logo"
    _organizationDTOCreatedAt <- o .: "createdAt"
    _organizationDTOUpdatedAt <- o .: "updatedAt"
    _organizationDTOLastAccessAt <- o .: "lastAccessAt"
    role <- o .: "role"
    case deserializeOrganizationRole role of
      (Just _organizationDTORole) -> return OrganizationDTO {..}
      Nothing -> fail "Unsupported organization role"
  parseJSON _ = mzero

instance ToJSON OrganizationDTO where
  toJSON OrganizationDTO {..} =
    object
      [ "organizationId" .= _organizationDTOOrganizationId
      , "name" .= _organizationDTOName
      , "description" .= _organizationDTODescription
      , "email" .= _organizationDTOEmail
      , "role" .= serializeOrganizationRole _organizationDTORole
      , "token" .= _organizationDTOToken
      , "active" .= _organizationDTOActive
      , "logo" .= _organizationDTOLogo
      , "createdAt" .= _organizationDTOCreatedAt
      , "updatedAt" .= _organizationDTOUpdatedAt
      , "lastAccessAt" .= _organizationDTOLastAccessAt
      ]
