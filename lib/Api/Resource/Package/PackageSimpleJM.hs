module Api.Resource.Package.PackageSimpleJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Organization.OrganizationSimpleJM ()

instance ToJSON PackageSimpleDTO where
  toJSON PackageSimpleDTO {..} =
    object
      [ "id" .= _packageSimpleDTOPId
      , "name" .= _packageSimpleDTOName
      , "organizationId" .= _packageSimpleDTOOrganizationId
      , "kmId" .= _packageSimpleDTOKmId
      , "version" .= _packageSimpleDTOVersion
      , "description" .= _packageSimpleDTODescription
      , "organization" .= _packageSimpleDTOOrganization
      , "createdAt" .= _packageSimpleDTOCreatedAt
      ]

instance FromJSON PackageSimpleDTO where
  parseJSON (Object o) = do
    _packageSimpleDTOPId <- o .: "id"
    _packageSimpleDTOName <- o .: "name"
    _packageSimpleDTOOrganizationId <- o .: "organizationId"
    _packageSimpleDTOKmId <- o .: "kmId"
    _packageSimpleDTOVersion <- o .: "version"
    _packageSimpleDTODescription <- o .: "description"
    _packageSimpleDTOOrganization <- o .: "organization"
    _packageSimpleDTOCreatedAt <- o .: "createdAt"
    return PackageSimpleDTO {..}
  parseJSON _ = mzero
