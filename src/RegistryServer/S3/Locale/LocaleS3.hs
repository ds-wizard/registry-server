module RegistryServer.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.Minio

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "locales"

retrieveLocale :: U.UUID -> String -> RequestContextM BS.ByteString
retrieveLocale localeUuid filename = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, filename])

retrieveLocale' :: U.UUID -> String -> RequestContextM (Either MinioErr BS.ByteString)
retrieveLocale' localeUuid filename = createGetObjectFn' (f' "%s/%s/%s" [folderName, U.toString localeUuid, filename])

putLocale :: U.UUID -> String -> BS.ByteString -> RequestContextM String
putLocale localeUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]) Nothing Nothing

removeLocales :: RequestContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: U.UUID -> RequestContextM ()
removeLocale localeUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString localeUuid])
