module RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Util.Aeson

instance ToJSON DocumentTemplateFormatDTO where
  toJSON DocumentTemplateFormatDTO {..} =
    object
      [ "uuid" .= uuid
      , "name" .= name
      , "shortName" .= name
      , "icon" .= icon
      , "color" .= "#FFFFFF"
      , "steps" .= steps
      ]

instance ToJSON DocumentTemplateFormatStepDTO where
  toJSON = genericToJSON jsonOptions
