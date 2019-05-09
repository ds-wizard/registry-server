module Api.Resource.Info.InfoDTO where

import Data.Aeson

data InfoDTO = InfoDTO
  { _infoDTOName :: String
  , _infoDTOVersion :: String
  , _infoDTOBuiltAt :: String
  }

instance ToJSON InfoDTO where
  toJSON InfoDTO {..} = object ["name" .= _infoDTOName, "version" .= _infoDTOVersion, "builtAt" .= _infoDTOBuiltAt]
