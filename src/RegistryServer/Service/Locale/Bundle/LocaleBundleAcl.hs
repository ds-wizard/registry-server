module RegistryServer.Service.Locale.Bundle.LocaleBundleAcl where

import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers

checkWritePermission :: RequestContextM ()
checkWritePermission = checkAdminRole "Write LocaleBundle"
