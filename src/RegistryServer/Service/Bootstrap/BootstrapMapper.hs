module RegistryServer.Service.Bootstrap.BootstrapMapper where

import RegistryServer.Api.Resource.Bootstrap.BootstrapDTO
import RegistryServer.Model.Config.ServerConfig

toBootstrapDTO :: ServerConfig -> BootstrapDTO
toBootstrapDTO serverConfig =
  BootstrapDTO
    { authentication = toBootstrapAuthenticationDTO serverConfig
    , locale = toBootstrapLocaleDTO serverConfig
    }

toBootstrapAuthenticationDTO :: ServerConfig -> BootstrapAuthenticationDTO
toBootstrapAuthenticationDTO serverConfig =
  BootstrapAuthenticationDTO
    { publicRegistrationEnabled = serverConfig.general.publicRegistrationEnabled
    }

toBootstrapLocaleDTO :: ServerConfig -> BootstrapLocaleDTO
toBootstrapLocaleDTO serverConfig =
  BootstrapLocaleDTO
    { enabled = serverConfig.general.localeEnabled
    }
