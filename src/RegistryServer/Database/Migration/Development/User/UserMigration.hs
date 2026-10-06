module RegistryServer.Database.Migration.Development.User.UserMigration where

import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.DAO.UserToken.UserTokenDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/User) started"
  insertUser userAdmin
  insertUser userNikola
  insertUserToken adminApiKey
  insertUserToken nikolaApiKey
  logInfo _CMP_MIGRATION "(Fixtures/User) ended"
