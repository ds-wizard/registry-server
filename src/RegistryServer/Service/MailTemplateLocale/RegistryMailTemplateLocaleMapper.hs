module RegistryServer.Service.MailTemplateLocale.RegistryMailTemplateLocaleMapper where

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Model.MailTemplateLocale.RegistryMailTemplateLocale

toDTO :: RegistryMailTemplateLocale -> LocaleDTO
toDTO locale =
  LocaleDTO
    { code = locale.code
    , name = locale.name
    , version = locale.version
    }
