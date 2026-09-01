module RegistryServer.Database.Migration.Development.PersistentCommand.Data.PersistentCommands where

import Shared.Constant.Tenant
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Date
import Shared.Util.Uuid

command1 :: PersistentCommand String
command1 =
  PersistentCommand
    { uuid = u' "34493424-ad08-4752-acf4-ac92223dc2f6"
    , state = DonePersistentCommandState
    , component = "component1"
    , function = "function1"
    , body = "{}"
    , lastTraceUuid = Nothing
    , lastErrorMessage = Nothing
    , attempts = 1
    , maxAttempts = 10
    , tenantUuid = defaultTenantUuid
    , createdBy = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
