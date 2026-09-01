module Specs.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Control.Monad.Reader (liftIO, runReaderT)

import RegistryServer.Model.Context.RequestContext

import SharedTest.Specs.Common

runInContext action requestContext =
  runExceptT . runStdoutLoggingT . filterLogger filterJustError $ runReaderT (runRequestContextM action) requestContext

runInContextIO action requestContext =
  liftIO . runExceptT $ runStdoutLoggingT . filterLogger filterJustError $ runReaderT (runRequestContextM action) requestContext
