module RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkSM where

import Data.Swagger

import RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Shared.Util.Swagger

instance ToSchema UserEmailLinkType

instance ToSchema (UserEmailLinkDTO UserEmailLinkType) where
  declareNamedSchema = toSwagger forgottenTokenUserEmailLinkDto
