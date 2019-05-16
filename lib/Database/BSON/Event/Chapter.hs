module Database.BSON.Event.Chapter where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.Event.EventPath ()
import LensesConfig
import Model.Event.Chapter.ChapterEvent

-- -------------------------
-- ADD CHAPTER EVENT--------
-- -------------------------
instance ToBSON AddChapterEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddChapterEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    ]

instance FromBSON AddChapterEvent where
  fromBSON doc = do
    chUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    chPath <- BSON.lookup "path" doc
    chChapterUuid <- deserializeMaybeUUID $ BSON.lookup "chapterUuid" doc
    chTitle <- BSON.lookup "title" doc
    chText <- BSON.lookup "text" doc
    return
      AddChapterEvent
      { _addChapterEventUuid = chUuid
      , _addChapterEventPath = chPath
      , _addChapterEventChapterUuid = chChapterUuid
      , _addChapterEventTitle = chTitle
      , _addChapterEventText = chText
      }

-- -------------------------
-- EDIT CHAPTER EVENT-------
-- -------------------------
instance ToBSON EditChapterEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditChapterEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "questionUuids" BSON.=: serializeEventFieldUUIDList (event ^. questionUuids)
    ]

instance FromBSON EditChapterEvent where
  fromBSON doc = do
    chUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    chPath <- BSON.lookup "path" doc
    chChapterUuid <- deserializeMaybeUUID $ BSON.lookup "chapterUuid" doc
    chTitle <- BSON.lookup "title" doc
    chText <- BSON.lookup "text" doc
    let chQuestionUuids = deserializeEventFieldUUIDList $ BSON.lookup "questionUuids" doc
    return
      EditChapterEvent
      { _editChapterEventUuid = chUuid
      , _editChapterEventPath = chPath
      , _editChapterEventChapterUuid = chChapterUuid
      , _editChapterEventTitle = chTitle
      , _editChapterEventText = chText
      , _editChapterEventQuestionUuids = chQuestionUuids
      }

-- -------------------------
-- DELETE CHAPTER EVENT-----
-- -------------------------
instance ToBSON DeleteChapterEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteChapterEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    ]

instance FromBSON DeleteChapterEvent where
  fromBSON doc = do
    chUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    chPath <- BSON.lookup "path" doc
    chChapterUuid <- deserializeMaybeUUID $ BSON.lookup "chapterUuid" doc
    return
      DeleteChapterEvent
      { _deleteChapterEventUuid = chUuid
      , _deleteChapterEventPath = chPath
      , _deleteChapterEventChapterUuid = chChapterUuid
      }
