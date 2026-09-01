module RegistryServer.Api.Handler.Swagger.Api where

import Data.Swagger
import qualified Data.Text as T
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import RegistryServer.Api.Handler.Api
import RegistryServer.Api.Resource.Config.ClientConfigSM ()
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM ()
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM ()
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailSM ()
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import RegistryServer.Api.Resource.Locale.LocaleDetailSM ()
import RegistryServer.Api.Resource.Locale.LocaleSM ()
import RegistryServer.Api.Resource.Organization.OrganizationChangeSM ()
import RegistryServer.Api.Resource.Organization.OrganizationCreateSM ()
import RegistryServer.Api.Resource.Organization.OrganizationSM ()
import RegistryServer.Api.Resource.Organization.OrganizationStateSM ()
import RegistryServer.Api.Resource.PersistentCommand.PersistentCommandSM ()
import RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkSM ()
import Shared.Api.Resource.Common.FileSM ()
import Shared.Api.Resource.Common.SemVer2TupleSM ()
import Shared.Api.Resource.Component.ComponentSM ()
import Shared.Api.Resource.Coordinate.CoordinateSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM ()
import Shared.Api.Resource.Info.InfoSM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageSM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: String -> Swagger
swagger version =
  let s = toSwagger applicationApi
   in s
        { _swaggerInfo =
            s._swaggerInfo
              { _infoTitle = "Registry API"
              , _infoDescription = Just "API specification for Registry"
              , _infoVersion = T.pack version
              , _infoLicense =
                  Just $
                    License
                      { _licenseName = "Apache-2.0"
                      , _licenseUrl = Just . URL $ "https://raw.githubusercontent.com/ds-wizard/engine-backend/main/LICENSE.md"
                      }
              }
        }

swaggerServer :: String -> Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer . swagger
