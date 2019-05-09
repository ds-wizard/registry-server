module Api.Resource.Package.PackageDetailDTO where

import Api.Resource.Organization.OrganizationSimpleDTO

data PackageDetailDTO = PackageDetailDTO
  { _packageDetailDTOPId :: String
  , _packageDetailDTOName :: String
  , _packageDetailDTOOrganizationId :: String
  , _packageDetailDTOKmId :: String
  , _packageDetailDTOVersion :: String
  , _packageDetailDTOMetamodelVersion :: Int
  , _packageDetailDTODescription :: String
  , _packageDetailDTOParentPackageId :: Maybe String
  , _packageDetailDTOVersions :: [String]
  , _packageDetailDTOOrganization :: OrganizationSimpleDTO
  } deriving (Show, Eq)
