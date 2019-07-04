module Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import Api.Resource.Organization.OrganizationSimpleDTO

data PackageDetailDTO = PackageDetailDTO
  { _packageDetailDTOPId :: String
  , _packageDetailDTOName :: String
  , _packageDetailDTOOrganizationId :: String
  , _packageDetailDTOKmId :: String
  , _packageDetailDTOVersion :: String
  , _packageDetailDTODescription :: String
  , _packageDetailDTOReadme :: String
  , _packageDetailDTOLicense :: String
  , _packageDetailDTOMetamodelVersion :: Int
  , _packageDetailDTOParentPackageId :: Maybe String
  , _packageDetailDTOVersions :: [String]
  , _packageDetailDTOOrganization :: OrganizationSimpleDTO
  , _packageDetailDTOCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)
