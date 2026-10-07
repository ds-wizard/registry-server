module RegistryServer.Database.DAO.Locale.RegistryLocaleDAO where

import GHC.Int

import RegistryServer.Database.Mapping.Locale.RegistryLocale ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Locale.RegistryLocale
import Shared.Database.DAO.Common

entityName = "locale"

findRegistryLocales :: RequestContextM [RegistryLocale]
findRegistryLocales = createFindEntitiesFn entityName

findRegistryLocaleByCodeAndVersion :: String -> String -> RequestContextM RegistryLocale
findRegistryLocaleByCodeAndVersion code version = createFindEntityByFn entityName [("code", code), ("version", version)]

insertRegistryLocale :: RegistryLocale -> RequestContextM Int64
insertRegistryLocale = createInsertFn entityName

deleteRegistryLocales :: RequestContextM Int64
deleteRegistryLocales = createDeleteEntitiesFn entityName
