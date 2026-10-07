module RegistryServer.Database.DAO.MailTemplateLocale.RegistryMailTemplateLocaleDAO where

import GHC.Int

import RegistryServer.Database.Mapping.MailTemplateLocale.RegistryMailTemplateLocale ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.MailTemplateLocale.RegistryMailTemplateLocale
import Shared.Database.DAO.Common

entityName = "mail_template_locale"

findRegistryMailTemplateLocales :: RequestContextM [RegistryMailTemplateLocale]
findRegistryMailTemplateLocales = createFindEntitiesFn entityName

findRegistryMailTemplateLocaleByCodeAndVersion :: String -> String -> RequestContextM RegistryMailTemplateLocale
findRegistryMailTemplateLocaleByCodeAndVersion code version = createFindEntityByFn entityName [("code", code), ("version", version)]

insertRegistryMailTemplateLocale :: RegistryMailTemplateLocale -> RequestContextM Int64
insertRegistryMailTemplateLocale = createInsertFn entityName

deleteRegistryMailTemplateLocales :: RequestContextM Int64
deleteRegistryMailTemplateLocales = createDeleteEntitiesFn entityName
