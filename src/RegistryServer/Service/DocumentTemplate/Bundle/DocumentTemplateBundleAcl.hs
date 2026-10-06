module RegistryServer.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAcl where

import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers

checkWritePermission :: RequestContextM ()
checkWritePermission = checkAdminRole "Write DocumentTemplateBundle"
