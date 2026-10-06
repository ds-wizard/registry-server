module RegistryServer.Service.Locale.LocaleMapper where

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Api.Resource.Locale.LocaleDetailDTO
import Shared.Model.Locale.Locale

toDTO :: Locale -> LocaleDTO
toDTO locale =
  LocaleDTO
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , createdAt = locale.createdAt
    }

toDetailDTO :: Locale -> [String] -> LocaleDetailDTO
toDetailDTO locale versions =
  LocaleDetailDTO
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , license = locale.license
    , readme = locale.readme
    , recommendedAppVersion = locale.recommendedAppVersion
    , versions = versions
    , createdAt = locale.createdAt
    }
