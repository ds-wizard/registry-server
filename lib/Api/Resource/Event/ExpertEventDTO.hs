module Api.Resource.Event.ExpertEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO

data AddExpertEventDTO = AddExpertEventDTO
  { _addExpertEventDTOUuid :: U.UUID
  , _addExpertEventDTOPath :: EventPathDTO
  , _addExpertEventDTOExpertUuid :: U.UUID
  , _addExpertEventDTOName :: String
  , _addExpertEventDTOEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEventDTO = EditExpertEventDTO
  { _editExpertEventDTOUuid :: U.UUID
  , _editExpertEventDTOPath :: EventPathDTO
  , _editExpertEventDTOExpertUuid :: U.UUID
  , _editExpertEventDTOName :: EventFieldDTO String
  , _editExpertEventDTOEmail :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteExpertEventDTO = DeleteExpertEventDTO
  { _deleteExpertEventDTOUuid :: U.UUID
  , _deleteExpertEventDTOPath :: EventPathDTO
  , _deleteExpertEventDTOExpertUuid :: U.UUID
  } deriving (Show, Eq, Generic)
