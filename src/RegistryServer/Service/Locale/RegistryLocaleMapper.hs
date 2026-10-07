module RegistryServer.Service.Locale.RegistryLocaleMapper where

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Model.Locale.RegistryLocale

toDTO :: RegistryLocale -> LocaleDTO
toDTO locale =
  LocaleDTO
    { code = locale.code
    , name = locale.name
    , version = locale.version
    }
