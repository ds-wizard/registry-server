module RegistryServer.Database.Migration.Development.MailTemplateLocale.Data.RegistryMailTemplateLocales where

import qualified Data.ByteString.Char8 as BS

import RegistryServer.Model.MailTemplateLocale.RegistryMailTemplateLocale
import Shared.Database.Migration.Development.Locale.Data.Locales (toPo)
import Shared.Util.Date
import Shared.Util.Uuid

registryMailTemplateLocaleDe :: RegistryMailTemplateLocale
registryMailTemplateLocaleDe =
  RegistryMailTemplateLocale
    { uuid = u' "5b0f3b8e-2a4c-4d6e-9f1a-7c3e8d2b6a41"
    , code = "de"
    , name = "Deutsch"
    , version = "5.0.0"
    , createdAt = dt' 2026 1 21
    }

registryMailTemplateLocaleDePo :: BS.ByteString
registryMailTemplateLocaleDePo = toPo "dsw-mail-templates:5.0.0" "de" "Deutsch" [("Settings", "Einstellungen")]
