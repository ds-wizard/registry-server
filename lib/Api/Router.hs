module Api.Router where

import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Text.Regex
import Web.Scotty.Trans
       (ScottyT, defaultHandler, delete, get, middleware, notFound, post,
        put)

import Api.Handler.ActionKey.ActionKeyHandler
import Api.Handler.Common
import Api.Handler.Info.InfoHandler
import Api.Handler.Organization.OrganizationHandler
import Api.Handler.Package.PackageHandler
import Api.Middleware.CORSMiddleware
import Api.Middleware.LoggingMiddleware
import LensesConfig
import Model.Context.BaseContext

unauthorizedEndpoints =
  [ (methodGet, mkRegex "^$")
  , (methodGet, mkRegex "^configuration$")
  , (methodGet, mkRegex "^export/.*$")
  , (methodPost, mkRegex "^users")
  , (methodPut, mkRegex "^users/.*/state")
  , (methodPut, mkRegex "^users/.*/password")
  , (methodPut, mkRegex "^users/.*/password?hash=.*")
  , (methodPost, mkRegex "^action-keys$")
  , (methodGet, mkRegex "^questionnaires/public$")
  , (methodGet, mkRegex "^questionnaires/.*/dmp")
  , (methodGet, mkRegex "^questionnaires/.*/dmp?format=.*$")
  , (methodGet, mkRegex "^book-references/.*")
  , (methodGet, mkRegex "^feedbacks.*")
  , (methodPost, mkRegex "^feedbacks.*")
  ]

createEndpoints :: BaseContext -> ScottyT Text BaseContextM ()
createEndpoints context
   ---------------------------
   -- MIDDLEWARES
   ---------------------------
 = do
  middleware (loggingMiddleware (context ^. appConfig . general . environment))
  middleware corsMiddleware
   ---------------------------
   -- ERROR HANDLING
   ---------------------------
  defaultHandler internalServerErrorA
   ---------------------------
   -- INFO
   ---------------------------
  get "/" getInfoA
   ---------------------------
   -- ORGANIZATION
   ---------------------------
  get "/organizations" getOrganizationsA
  post "/organizations" postOrganizationsA
  get "/organizations/:orgId" getOrganizationA
  put "/organizations/:orgId" putOrganizationA
  delete "/organizations/:orgId" deleteOrganizationA
  put "/organizations/:orgId/state" changeOrganizationStateA
  put "/organizations/:orgId/token" putOrganizationTokenA
   ---------------------------
   -- PACKAGE
   ---------------------------
  get "/packages/unique" getUniquePackagesA
  get "/packages/:pkgId" getPackageA
  get "/packages/:pkgId/bundle" getPackageBundleA
   ---------------------------
   -- ACTION KEY
   ---------------------------
  post "/action-keys" postActionKeysA
   ---------------------------
   -- ERROR
   ---------------------------
  notFound notFoundA
