module Api.Resource.Package.PackageJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Event.EventDTO ()
import Api.Resource.Package.PackageDTO

instance FromJSON PackageDTO where
  parseJSON (Object o) = do
    _packageDTOPId <- o .: "id"
    _packageDTOName <- o .: "name"
    _packageDTOOrganizationId <- o .: "organizationId"
    _packageDTOKmId <- o .: "kmId"
    _packageDTOVersion <- o .: "version"
    _packageDTOMetamodelVersion <- o .: "metamodelVersion"
    _packageDTODescription <- o .: "description"
    _packageDTOReadme <- o .: "readme"
    _packageDTOParentPackageId <- o .: "parentPackageId"
    eventSerialized <- o .: "events"
    _packageDTOEvents <- parseJSON eventSerialized
    _packageDTOCreatedAt <- o .: "createdAt"
    return PackageDTO {..}
  parseJSON _ = mzero

instance ToJSON PackageDTO where
  toJSON PackageDTO {..} =
    object
      [ "id" .= _packageDTOPId
      , "name" .= _packageDTOName
      , "organizationId" .= _packageDTOOrganizationId
      , "kmId" .= _packageDTOKmId
      , "version" .= _packageDTOVersion
      , "metamodelVersion" .= _packageDTOMetamodelVersion
      , "description" .= _packageDTODescription
      , "readme" .= _packageDTOReadme
      , "parentPackageId" .= _packageDTOParentPackageId
      , "events" .= toJSON _packageDTOEvents
      , "createdAt" .= _packageDTOCreatedAt
      ]
