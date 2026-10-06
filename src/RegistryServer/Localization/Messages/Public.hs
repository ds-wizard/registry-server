module RegistryServer.Localization.Messages.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- API
-- --------------------------------------
-- Common
_ERROR_API_COMMON__UNABLE_TO_GET_USER =
  LocaleRecord "error.api.common.unable_to_get_user" "Unable to get user from token header" []

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Uniqueness
_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email =
  LocaleRecord "error.validation.user_email_uniqueness" "Email ('%s') already exists" [email]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD =
  LocaleRecord "error.service.token.incorrect_email_or_password" "Incorrect email or password" []

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED =
  LocaleRecord "error.service.token.account_is_not_activated" "The account is not activated" []

-- --------------------------------------
-- MODEL
-- --------------------------------------
-- RequestContext
_ERROR_MODEL_APP_CONTEXT__MISSING_USER =
  LocaleRecord "error.model.app_context.missing_user" "You have to be log in to run." []
