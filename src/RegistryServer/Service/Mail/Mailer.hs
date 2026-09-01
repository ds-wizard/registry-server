module RegistryServer.Service.Mail.Mailer (
  sendRegistrationConfirmationMail,
  sendRegistrationCreatedAnalyticsMail,
  sendResetTokenMail,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Model.Config.ServerConfig
import qualified Shared.Model.PersistentCommand.Mail.MailCommand as MC
import Shared.Service.PersistentCommand.PersistentCommandMapper
import qualified Shared.Util.Aeson as A
import Shared.Util.JSON
import Shared.Util.Uuid

sendRegistrationConfirmationMail :: OrganizationDTO -> String -> Maybe String -> RequestContextM ()
sendRegistrationConfirmationMail org hash mCallbackUrl = do
  serverConfig <- asks serverConfig
  let clientAddress = serverConfig.general.clientUrl
  runInTransaction $ do
    let body =
          MC.MailCommand
            { mode = "registry"
            , template = "registrationConfirmation"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = org.email}]
            , parameters =
                M.fromList
                  [ ("organizationId", A.string org.organizationId)
                  , ("organizationName", A.string org.name)
                  , ("organizationEmail", A.string org.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientAddress)
                  , ("callbackUrl", A.maybeString mCallbackUrl)
                  ]
            }
    sendEmail body org.organizationId

sendRegistrationCreatedAnalyticsMail :: OrganizationDTO -> RequestContextM ()
sendRegistrationCreatedAnalyticsMail org =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let clientAddress = serverConfig.general.clientUrl
    let body =
          MC.MailCommand
            { mode = "registry"
            , template = "registrationCreatedAnalytics"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = serverConfig.analyticalMails.email}]
            , parameters =
                M.fromList
                  [ ("organizationId", A.string org.organizationId)
                  , ("organizationName", A.string org.name)
                  , ("organizationEmail", A.string org.email)
                  , ("clientUrl", A.string clientAddress)
                  ]
            }
    sendEmail body org.organizationId

sendResetTokenMail :: OrganizationDTO -> String -> RequestContextM ()
sendResetTokenMail org hash =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let clientAddress = serverConfig.general.clientUrl
    let body =
          MC.MailCommand
            { mode = "registry"
            , template = "resetToken"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = org.email}]
            , parameters =
                M.fromList
                  [ ("organizationId", A.string org.organizationId)
                  , ("organizationName", A.string org.name)
                  , ("organizationEmail", A.string org.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientAddress)
                  ]
            }
    sendEmail body org.organizationId

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => dto -> String -> RequestContextM ()
sendEmail dto createdBy = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand uuid "mailer" "sendMail" body 10 U.nil (Just createdBy) now
    insertPersistentCommand command
    return ()
