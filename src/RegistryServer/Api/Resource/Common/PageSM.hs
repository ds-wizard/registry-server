module RegistryServer.Api.Resource.Common.PageSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserSM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Model.Common.Page
import Shared.Util.Swagger

instance ToSchema (Page UserDTO) where
  declareNamedSchema = toSwaggerWithDtoName "Page UserDTO" (Page "users" pageMetadata [userAdminDTO])
