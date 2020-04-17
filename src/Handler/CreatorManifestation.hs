{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.CreatorManifestation where

import Import
import Data.Aeson.Types ( Result(..) )

getCreatorManifestationsR :: Handler Value
getCreatorManifestationsR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity CreatorManifestation]
  returnJson $ queryResult

postCreatorManifestationsR :: Handler Value
postCreatorManifestationsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: CreatorManifestation)

  returnJson insertResult

deleteCreatorManifestationR :: CreatorManifestationId -> Handler Value
deleteCreatorManifestationR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result
