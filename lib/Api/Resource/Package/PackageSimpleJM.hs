module Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleJM ()
import Api.Resource.Package.PackageSimpleDTO

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
