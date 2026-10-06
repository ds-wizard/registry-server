module RegistryServer.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAcl where

import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers

checkWritePermission :: RequestContextM ()
checkWritePermission = checkAdminRole "Write KnowledgeModelBundle"
