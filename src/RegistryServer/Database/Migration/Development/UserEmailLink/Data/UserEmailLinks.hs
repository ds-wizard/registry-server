module RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.User.User
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Model.UserEmailLink.UserEmailLink

registrationUserEmailLink =
  UserEmailLink
    { uuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , identity = U.toString userAdmin.uuid
    , aType = RegistrationUserEmailLinkType
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenPasswordUserEmailLink =
  UserEmailLink
    { uuid = fromJust . U.fromString $ "2728460f-ba9a-4a05-8e47-7faa4dc931bf"
    , identity = U.toString userAdmin.uuid
    , aType = ForgottenPasswordUserEmailLinkType
    , hash = "5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenPasswordUserEmailLinkDto =
  UserEmailLinkDTO {aType = forgottenPasswordUserEmailLink.aType, email = userAdmin.email}
