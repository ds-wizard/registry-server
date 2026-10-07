module RegistryServer.Database.Migration.Development.Locale.Data.RegistryLocales where

import qualified Data.ByteString.Char8 as BS

import RegistryServer.Model.Locale.RegistryLocale
import Shared.Database.Migration.Development.Locale.Data.Locales (toPo)
import Shared.Util.Date
import Shared.Util.Uuid

registryLocaleDe :: RegistryLocale
registryLocaleDe =
  RegistryLocale
    { uuid = u' "0e5a5ab6-7d0c-4f86-9a8f-4b6f0dbd8e2c"
    , code = "de"
    , name = "Deutsch"
    , version = "5.0.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleDe400 :: RegistryLocale
registryLocaleDe400 =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a01"
    , code = "de"
    , name = "Deutsch"
    , version = "4.0.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleDe510 :: RegistryLocale
registryLocaleDe510 =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a02"
    , code = "de"
    , name = "Deutsch"
    , version = "5.1.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleCs600 :: RegistryLocale
registryLocaleCs600 =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a03"
    , code = "cs"
    , name = "Čeština"
    , version = "6.0.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleCs520 :: RegistryLocale
registryLocaleCs520 =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a04"
    , code = "cs"
    , name = "Čeština"
    , version = "5.2.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleNlNext :: RegistryLocale
registryLocaleNlNext =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a05"
    , code = "nl"
    , name = "Nederlands"
    , version = "next"
    , createdAt = dt' 2026 1 21
    }

registryLocaleNl4100 :: RegistryLocale
registryLocaleNl4100 =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a06"
    , code = "nl"
    , name = "Nederlands"
    , version = "4.10.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleNl490 :: RegistryLocale
registryLocaleNl490 =
  RegistryLocale
    { uuid = u' "1d6f6f0e-3a4b-4a54-8d1c-9f1f0c3b5a07"
    , code = "nl"
    , name = "Nederlands"
    , version = "4.9.0"
    , createdAt = dt' 2026 1 21
    }

registryLocaleVersions :: [RegistryLocale]
registryLocaleVersions =
  [ registryLocaleDe400
  , registryLocaleDe510
  , registryLocaleCs600
  , registryLocaleCs520
  , registryLocaleNlNext
  , registryLocaleNl4100
  , registryLocaleNl490
  ]

registryLocaleDePo :: BS.ByteString
registryLocaleDePo = toPo "dsw-client:5.0.0" "de" "Deutsch" [("Projects", "Projekte"), ("Settings", "Einstellungen")]
