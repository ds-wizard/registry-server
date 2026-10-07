module RegistryServer.S3.Locale.RegistryLocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Constant.Gettext
import Shared.S3.Common
import Shared.Util.String (f')

retrieveRegistryLocalePo :: U.UUID -> RequestContextM BS.ByteString
retrieveRegistryLocalePo = createGetObjectFn . poObjectName

putRegistryLocalePo :: U.UUID -> BS.ByteString -> RequestContextM String
putRegistryLocalePo uuid = createPutObjectFn (poObjectName uuid) (Just poContentType) Nothing

poObjectName :: U.UUID -> String
poObjectName uuid = f' "locales/%s/ui.po" [U.toString uuid]
