module Api.Resource.Package.PackageDetailJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleJM ()
import Api.Resource.Package.PackageDetailDTO

instance ToJSON PackageDetailDTO where
  toJSON PackageDetailDTO {..} =
    object
      [ "id" .= _packageDetailDTOPId
      , "name" .= _packageDetailDTOName
      , "organizationId" .= _packageDetailDTOOrganizationId
      , "kmId" .= _packageDetailDTOKmId
      , "version" .= _packageDetailDTOVersion
      , "metamodelVersion" .= _packageDetailDTOMetamodelVersion
      , "description" .= _packageDetailDTODescription
      , "readme" .= _packageDetailDTOReadme
      , "parentPackageId" .= _packageDetailDTOParentPackageId
      , "versions" .= _packageDetailDTOVersions
      , "organization" .= _packageDetailDTOOrganization
      , "createdAt" .= _packageDetailDTOCreatedAt
      ]

instance FromJSON PackageDetailDTO where
  parseJSON (Object o) = do
    _packageDetailDTOPId <- o .: "id"
    _packageDetailDTOName <- o .: "name"
    _packageDetailDTOOrganizationId <- o .: "organizationId"
    _packageDetailDTOKmId <- o .: "kmId"
    _packageDetailDTOVersion <- o .: "version"
    _packageDetailDTOMetamodelVersion <- o .: "metamodelVersion"
    _packageDetailDTODescription <- o .: "description"
    _packageDetailDTOReadme <- o .: "readme"
    _packageDetailDTOParentPackageId <- o .: "parentPackageId"
    _packageDetailDTOVersions <- o .: "versions"
    _packageDetailDTOOrganization <- o .: "organization"
    _packageDetailDTOCreatedAt <- o .: "createdAt"
    return PackageDetailDTO {..}
  parseJSON _ = mzero
