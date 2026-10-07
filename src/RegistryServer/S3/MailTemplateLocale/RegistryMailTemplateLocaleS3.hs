module RegistryServer.S3.MailTemplateLocale.RegistryMailTemplateLocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Constant.Gettext
import Shared.S3.Common
import Shared.Util.String (f')

retrieveRegistryMailTemplateLocalePo :: U.UUID -> RequestContextM BS.ByteString
retrieveRegistryMailTemplateLocalePo = createGetObjectFn . poObjectName

putRegistryMailTemplateLocalePo :: U.UUID -> BS.ByteString -> RequestContextM String
putRegistryMailTemplateLocalePo uuid = createPutObjectFn (poObjectName uuid) (Just poContentType) Nothing

poObjectName :: U.UUID -> String
poObjectName uuid = f' "mail-template-locales/%s/mail.po" [U.toString uuid]
