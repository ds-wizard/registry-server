module RegistryServer.Service.Locale.LocaleValidation where

import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Common

checkIfLocaleEnabled :: RequestContextM ()
checkIfLocaleEnabled = checkIfServerFeatureIsEnabled "Locale" (\s -> s.general.localeEnabled)
