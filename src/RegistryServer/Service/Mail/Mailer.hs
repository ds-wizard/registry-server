module RegistryServer.Service.Mail.Mailer (
  sendRegistrationConfirmationMail,
  sendRegistrationCreatedAnalyticsMail,
  sendResetPasswordMail,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON, Value)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Model.Config.ServerConfig
import qualified Shared.Model.PersistentCommand.Mail.MailCommand as MC
import Shared.Service.PersistentCommand.PersistentCommandMapper
import qualified Shared.Util.Aeson as A
import Shared.Util.JSON
import Shared.Util.Uuid

sendRegistrationConfirmationMail :: User -> String -> RequestContextM ()
sendRegistrationConfirmationMail user hash = do
  serverConfig <- asks (.serverConfig)
  let parameters = userParameters user serverConfig ++ [("hash", A.string hash)]
  sendEmail (toMailCommand "registrationConfirmation" user.email parameters) user

sendRegistrationCreatedAnalyticsMail :: User -> RequestContextM ()
sendRegistrationCreatedAnalyticsMail user = do
  serverConfig <- asks (.serverConfig)
  let parameters = userParameters user serverConfig
  sendEmail (toMailCommand "registrationCreatedAnalytics" serverConfig.analyticalMails.email parameters) user

sendResetPasswordMail :: User -> String -> RequestContextM ()
sendResetPasswordMail user hash = do
  serverConfig <- asks (.serverConfig)
  let parameters = userParameters user serverConfig ++ [("hash", A.string hash)]
  sendEmail (toMailCommand "resetPassword" user.email parameters) user

-- --------------------------------
-- PRIVATE
-- --------------------------------
toMailCommand :: String -> String -> [(String, Value)] -> MC.MailCommand
toMailCommand template email parameters =
  MC.MailCommand
    { mode = "registry"
    , template = template
    , recipients = [MC.MailRecipient {uuid = Nothing, email = email}]
    , parameters = M.fromList parameters
    }

userParameters :: User -> ServerConfig -> [(String, Value)]
userParameters user serverConfig =
  [ ("userUuid", A.string . U.toString $ user.uuid)
  , ("userFirstName", A.string user.firstName)
  , ("userLastName", A.string user.lastName)
  , ("userEmail", A.string user.email)
  , ("clientUrl", A.string serverConfig.general.clientUrl)
  ]

sendEmail :: ToJSON dto => dto -> User -> RequestContextM ()
sendEmail dto user = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand uuid "mailer" "sendMail" body 10 U.nil (Just . U.toString $ user.uuid) now
    insertPersistentCommand command
    return ()
