module Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Control.Monad.Reader (ask)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Integration.Aws.Lambda
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (trim)
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.LambdaInvocationResult ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommandSimple ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

entityName = "persistent_command"

channelName = "persistent_command_channel"

componentCondition :: [String] -> String
componentCondition components = f' "component IN (%s)" [generateQuestionMarks components]

findPersistentCommands :: (AppContextC s sc m, FromField identity) => m [PersistentCommand identity]
findPersistentCommands = do
  createFindEntitiesFn entityName

findPersistentCommandsForRetryByStates :: (AppContextC s sc m, FromField identity) => [String] -> m [PersistentCommandSimple identity]
findPersistentCommandsForRetryByStates [] = return []
findPersistentCommandsForRetryByStates components = do
  let sql =
        fromString $
          f'
            "SELECT uuid, component, tenant_uuid, created_by \
            \FROM %s \
            \WHERE (state = 'NewPersistentCommandState' \
            \  OR (state = 'ErrorPersistentCommandState' AND attempts < max_attempts AND updated_at < (now() - (2 ^ attempts - 1) * INTERVAL '1 min'))) \
            \  AND %s \
            \ORDER BY created_at \
            \LIMIT 5 \
            \FOR UPDATE"
            [entityName, componentCondition components]
  let params = components
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findPersistentCommandByUuid :: (AppContextC s sc m, FromField identity) => U.UUID -> m (PersistentCommand identity)
findPersistentCommandByUuid uuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", U.toString uuid)]

findPersistentCommandByUuid' :: (AppContextC s sc m, FromField identity) => U.UUID -> m (Maybe (PersistentCommand identity))
findPersistentCommandByUuid' uuid = createFindEntityWithFieldsByFn' "*" entityName [("uuid", U.toString uuid)]

findPersistentCommandSimpleByUuid :: (AppContextC s sc m, FromField identity) => U.UUID -> m (PersistentCommandSimple identity)
findPersistentCommandSimpleByUuid uuid = createFindEntityWithFieldsByFn "uuid, component, tenant_uuid, created_by" False entityName [("uuid", U.toString uuid)]

insertPersistentCommand :: (AppContextC s sc m, ToField identity) => PersistentCommand identity -> m Int64
insertPersistentCommand command = do
  createInsertFn entityName command
  context <- ask
  case L.find (\lf -> lf.component == command.component) context.serverConfig'.persistentCommand'.lambdaFunctions of
    Just lf -> invokeLambdaFunction lf
    Nothing -> notifyPersistentCommandQueues command

updatePersistentCommandByUuid :: (AppContextC s sc m, ToField identity) => PersistentCommand identity -> m Int64
updatePersistentCommandByUuid command = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, state = ?, component = ?, function = ?, body = ?, last_error_message = ?, attempts = ?, max_attempts = ?, tenant_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, last_trace_uuid = ? WHERE uuid = ? AND tenant_uuid = ? AND state != 'DonePersistentCommandState'"
            [entityName]
  let params = toRow command ++ [toField command.uuid, toField command.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePersistentCommands :: AppContextC s sc m => m Int64
deletePersistentCommands = createDeleteEntitiesFn entityName

deletePersistentCommandsByCreatedBy :: AppContextC s sc m => [U.UUID] -> m Int64
deletePersistentCommandsByCreatedBy createdBys = do
  createDeleteEntityWhereInFn entityName "created_by" (fmap U.toString createdBys)

deletePersistentCommandByUuid :: AppContextC s sc m => U.UUID -> m Int64
deletePersistentCommandByUuid uuid = do
  createDeleteEntityByFn entityName [("uuid", U.toString uuid)]

listenPersistentCommandChannel :: AppContextC s sc m => m ()
listenPersistentCommandChannel = createChannelListener channelName

createChannelListener :: AppContextC s sc m => String -> m ()
createChannelListener name = do
  let sql = f' "LISTEN %s" [name]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  logInfoI _CMP_DATABASE (f' "Listening for '%s' channel" [name])

getChannelNotification :: AppContextC s sc m => m Notification
getChannelNotification = do
  logInfoI _CMP_DATABASE "Waiting for new notification"
  notification <- runDB getNotification
  logInfoI _CMP_DATABASE (f' "Receiving notification for channel '%s'" [BS.unpack . notificationChannel $ notification])
  return notification

notifyPersistentCommandQueue :: AppContextC s sc m => m Int64
notifyPersistentCommandQueue = do
  let sql = f' "NOTIFY %s" [channelName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

notifyPersistentCommandQueues :: AppContextC s sc m => PersistentCommand identity -> m Int64
notifyPersistentCommandQueues command = do
  notifyPersistentCommandQueue
  notifySpecificPersistentCommandQueue command

notifySpecificPersistentCommandQueue :: AppContextC s sc m => PersistentCommand identity -> m Int64
notifySpecificPersistentCommandQueue command = do
  let sql = f' "NOTIFY %s__%s, '%s'" [channelName, command.component, U.toString command.uuid]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

invokeLambdaFunction :: AppContextC s sc m => ServerConfigPersistentCommandLambda -> m Int64
invokeLambdaFunction lf = do
  invokeLambda lf.functionArn "{}"
  return 1
