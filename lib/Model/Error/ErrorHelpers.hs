module Model.Error.ErrorHelpers where

import Model.Error.Error

createErrorWithErrorMessage errorMessage = ValidationError errorMessage [] []

createErrorWithFormError formError = ValidationError "" [formError] []

createErrorWithFieldError fieldError = ValidationError "" [] [fieldError]

addErrorMessage (ValidationError errorMessage formErrors fieldErrors) newErrorMessage =
  ValidationError newErrorMessage formErrors fieldErrors

addFormError (ValidationError errorMessage formErrors fieldErrors) newFormError =
  ValidationError errorMessage (formErrors ++ [newFormError]) fieldErrors

addFieldError (ValidationError errorMessage formErrors fieldErrors) newFieldError =
  ValidationError errorMessage formErrors (fieldErrors ++ [newFieldError])

clearErrorMessage (ValidationError errorMessage formErrors fieldErrors) = ValidationError "" formErrors fieldErrors

clearFormError (ValidationError errorMessage formErrors fieldErrors) = ValidationError errorMessage [] fieldErrors

clearFieldError (ValidationError errorMessage formErrors fieldErrors) = ValidationError errorMessage formErrors []
