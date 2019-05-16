module Database.BSON.ActionKey.ActionKey where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Model.ActionKey.ActionKey

instance ToBSON ActionKey where
  toBSON ActionKey {..} =
    [ "uuid" BSON.=: serializeUUID _actionKeyUuid
    , "organizationId" BSON.=: _actionKeyOrganizationId
    , "type" BSON.=: serializeActionType _actionKeyAType
    , "hash" BSON.=: _actionKeyHash
    , "createdAt" BSON.=: _actionKeyCreatedAt
    ]

instance FromBSON ActionKey where
  fromBSON doc = do
    _actionKeyUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    _actionKeyOrganizationId <- BSON.lookup "organizationId" doc
    _actionKeyAType <- deserializeActionType $ BSON.lookup "type" doc
    _actionKeyHash <- BSON.lookup "hash" doc
    _actionKeyCreatedAt <- BSON.lookup "createdAt" doc
    return ActionKey {..}
