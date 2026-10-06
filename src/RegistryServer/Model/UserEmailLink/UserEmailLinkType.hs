module RegistryServer.Model.UserEmailLink.UserEmailLinkType where

import GHC.Generics

data UserEmailLinkType
  = RegistrationUserEmailLinkType
  | ForgottenPasswordUserEmailLinkType
  deriving (Show, Eq, Generic, Read)
