module RegistryServer.Service.Common where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import RegistryServer.Model.Context.RequestContext
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

checkIfServerFeatureIsEnabled featureName accessor = do
  serverConfig <- asks serverConfig
  if accessor serverConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
