module RegistryServer.Api.Resource.Bootstrap.BootstrapDTO where

import GHC.Generics

data BootstrapDTO = BootstrapDTO
  { authentication :: BootstrapAuthenticationDTO
  , locale :: BootstrapLocaleDTO
  }
  deriving (Show, Eq, Generic)

data BootstrapAuthenticationDTO = BootstrapAuthenticationDTO
  { publicRegistrationEnabled :: Bool
  }
  deriving (Generic, Eq, Show)

data BootstrapLocaleDTO = BootstrapLocaleDTO
  { enabled :: Bool
  }
  deriving (Generic, Eq, Show)
