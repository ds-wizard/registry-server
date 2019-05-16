module Database.BSON.Event.Tag where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.Event.EventPath ()
import LensesConfig
import Model.Event.Tag.TagEvent

-- -------------------------
-- ADD TAG EVENT -----------
-- -------------------------
instance ToBSON AddTagEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddTagEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "tagUuid" BSON.=: serializeUUID (event ^. tagUuid)
    , "name" BSON.=: (event ^. name)
    , "description" BSON.=: (event ^. description)
    , "color" BSON.=: (event ^. color)
    ]

instance FromBSON AddTagEvent where
  fromBSON doc = do
    tUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    tPath <- BSON.lookup "path" doc
    tTagUuid <- deserializeMaybeUUID $ BSON.lookup "tagUuid" doc
    tName <- BSON.lookup "name" doc
    tDescription <- BSON.lookup "description" doc
    tColor <- BSON.lookup "color" doc
    return
      AddTagEvent
      { _addTagEventUuid = tUuid
      , _addTagEventPath = tPath
      , _addTagEventTagUuid = tTagUuid
      , _addTagEventName = tName
      , _addTagEventDescription = tDescription
      , _addTagEventColor = tColor
      }

-- -------------------------
-- EDIT TAG EVENT ----------
-- -------------------------
instance ToBSON EditTagEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditTagEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "tagUuid" BSON.=: serializeUUID (event ^. tagUuid)
    , "name" BSON.=: (event ^. name)
    , "description" BSON.=: (event ^. description)
    , "color" BSON.=: (event ^. color)
    ]

instance FromBSON EditTagEvent where
  fromBSON doc = do
    tUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    tPath <- BSON.lookup "path" doc
    tTagUuid <- deserializeMaybeUUID $ BSON.lookup "tagUuid" doc
    tName <- BSON.lookup "name" doc
    tDescription <- BSON.lookup "description" doc
    tColor <- BSON.lookup "color" doc
    return
      EditTagEvent
      { _editTagEventUuid = tUuid
      , _editTagEventPath = tPath
      , _editTagEventTagUuid = tTagUuid
      , _editTagEventName = tName
      , _editTagEventDescription = tDescription
      , _editTagEventColor = tColor
      }

-- -------------------------
-- DELETE TAG EVENT --------
-- -------------------------
instance ToBSON DeleteTagEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteTagEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "tagUuid" BSON.=: serializeUUID (event ^. tagUuid)
    ]

instance FromBSON DeleteTagEvent where
  fromBSON doc = do
    tUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    tPath <- BSON.lookup "path" doc
    tTagUuid <- deserializeMaybeUUID $ BSON.lookup "tagUuid" doc
    return DeleteTagEvent {_deleteTagEventUuid = tUuid, _deleteTagEventPath = tPath, _deleteTagEventTagUuid = tTagUuid}
