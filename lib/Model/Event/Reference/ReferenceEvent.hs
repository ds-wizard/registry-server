module Model.Event.Reference.ReferenceEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddReferenceEvent
  = AddResourcePageReferenceEvent' AddResourcePageReferenceEvent
  | AddURLReferenceEvent' AddURLReferenceEvent
  | AddCrossReferenceEvent' AddCrossReferenceEvent
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEvent = AddResourcePageReferenceEvent
  { _addResourcePageReferenceEventUuid :: U.UUID
  , _addResourcePageReferenceEventPath :: EventPath
  , _addResourcePageReferenceEventReferenceUuid :: U.UUID
  , _addResourcePageReferenceEventShortUuid :: String
  } deriving (Show, Eq, Generic)

data AddURLReferenceEvent = AddURLReferenceEvent
  { _addURLReferenceEventUuid :: U.UUID
  , _addURLReferenceEventPath :: EventPath
  , _addURLReferenceEventReferenceUuid :: U.UUID
  , _addURLReferenceEventUrl :: String
  , _addURLReferenceEventLabel :: String
  } deriving (Show, Eq, Generic)

data AddCrossReferenceEvent = AddCrossReferenceEvent
  { _addCrossReferenceEventUuid :: U.UUID
  , _addCrossReferenceEventPath :: EventPath
  , _addCrossReferenceEventReferenceUuid :: U.UUID
  , _addCrossReferenceEventTargetUuid :: U.UUID
  , _addCrossReferenceEventDescription :: String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditReferenceEvent
  = EditResourcePageReferenceEvent' EditResourcePageReferenceEvent
  | EditURLReferenceEvent' EditURLReferenceEvent
  | EditCrossReferenceEvent' EditCrossReferenceEvent
  deriving (Show, Eq, Generic)

data EditResourcePageReferenceEvent = EditResourcePageReferenceEvent
  { _editResourcePageReferenceEventUuid :: U.UUID
  , _editResourcePageReferenceEventPath :: EventPath
  , _editResourcePageReferenceEventReferenceUuid :: U.UUID
  , _editResourcePageReferenceEventShortUuid :: EventField String
  } deriving (Show, Eq, Generic)

data EditURLReferenceEvent = EditURLReferenceEvent
  { _editURLReferenceEventUuid :: U.UUID
  , _editURLReferenceEventPath :: EventPath
  , _editURLReferenceEventReferenceUuid :: U.UUID
  , _editURLReferenceEventUrl :: EventField String
  , _editURLReferenceEventLabel :: EventField String
  } deriving (Show, Eq, Generic)

data EditCrossReferenceEvent = EditCrossReferenceEvent
  { _editCrossReferenceEventUuid :: U.UUID
  , _editCrossReferenceEventPath :: EventPath
  , _editCrossReferenceEventReferenceUuid :: U.UUID
  , _editCrossReferenceEventTargetUuid :: EventField U.UUID
  , _editCrossReferenceEventDescription :: EventField String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEvent = DeleteReferenceEvent
  { _deleteReferenceEventUuid :: U.UUID
  , _deleteReferenceEventPath :: EventPath
  , _deleteReferenceEventReferenceUuid :: U.UUID
  } deriving (Show, Eq, Generic)
