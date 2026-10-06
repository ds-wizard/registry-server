module RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation (
  validateIsVersionHigher,
) where

import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Error.Error
import Shared.Util.Reference

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
