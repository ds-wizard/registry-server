module RegistryServer.S3.DocumentTemplate.DocumentTemplateS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "document-templates"

retrieveAsset :: U.UUID -> U.UUID -> RequestContextM BS.ByteString
retrieveAsset documentTemplateUuid assetUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

putAsset :: U.UUID -> U.UUID -> String -> BS.ByteString -> RequestContextM String
putAsset documentTemplateUuid assetUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid]) (Just contentType) Nothing

makePublicLink :: U.UUID -> U.UUID -> RequestContextM String
makePublicLink documentTemplateUuid assetUuid = createMakePublicLink (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

removeAssets :: U.UUID -> RequestContextM ()
removeAssets documentTemplateUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString documentTemplateUuid])

removeAsset :: U.UUID -> U.UUID -> RequestContextM ()
removeAsset documentTemplateUuid assetUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

makeBucket :: RequestContextM ()
makeBucket = createMakeBucketFn

purgeBucket :: RequestContextM ()
purgeBucket = createPurgeBucketFn

removeBucket :: RequestContextM ()
removeBucket = createRemoveBucketFn
