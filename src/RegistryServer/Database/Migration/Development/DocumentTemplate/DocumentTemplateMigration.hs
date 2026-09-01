module RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/DocumentTemplate) started"
  deleteDocumentTemplates
  purgeBucket
  insertDocumentTemplate wizardDocumentTemplate
  insertDocumentTemplateFormat formatJson
  insertDocumentTemplateFormat formatHtml
  insertDocumentTemplateFormat formatPdf
  insertDocumentTemplateFormat formatLatex
  insertDocumentTemplateFormat formatDocx
  insertDocumentTemplateFormat formatOdt
  insertDocumentTemplateFormat formatMarkdown
  insertFile fileDefaultHtml
  insertFile fileDefaultCss
  insertAsset assetLogo
  putAsset wizardDocumentTemplate.uuid assetLogo.uuid assetLogo.contentType assetLogoContent
  logInfo _CMP_MIGRATION "(Fixtures/DocumentTemplate) ended"
