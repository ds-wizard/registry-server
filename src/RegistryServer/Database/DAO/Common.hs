module RegistryServer.Database.DAO.Common (
  module Shared.Database.DAO.Common,
  runInTransaction,
) where

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Database.DAO.Common as S
import Shared.Util.Logger

runInTransaction :: RequestContextM a -> RequestContextM a
runInTransaction = S.runInTransaction logInfoI logWarnI
