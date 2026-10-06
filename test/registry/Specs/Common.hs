module Specs.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Control.Monad.Reader (liftIO, runReaderT)

import RegistryServer.Model.Context.RequestContext

import SharedTest.Specs.Common

runInContext (RequestContextM action) requestContext =
  runExceptT . runStdoutLoggingT . filterLogger filterJustError $ runReaderT action requestContext

runInContextIO (RequestContextM action) requestContext =
  liftIO . runExceptT $ runStdoutLoggingT . filterLogger filterJustError $ runReaderT action requestContext
