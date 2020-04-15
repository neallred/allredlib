{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Manifestation where

import Import
import Data.Aeson.Types ( Result(..) )

getManifestationsR :: Handler Value
getManifestationsR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity Manifestation]
  returnJson $ queryResult

postManifestationsR :: Handler Value
postManifestationsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Manifestation)

  returnJson insertResult

getManifestationR :: ManifestationId -> Handler Value
getManifestationR resourceId = do
  queryResult <- runDB $ selectFirst [ManifestationId ==. resourceId] [] :: Handler (Maybe (Entity Manifestation))
  returnJson $ queryResult

putManifestationR :: ManifestationId -> Handler Value
putManifestationR resourceId = do
  updatedResource <- parseCheckJsonBody
  result <- case updatedResource of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace resourceId x
  returnJson [result]

deleteManifestationR :: ManifestationId -> Handler Value
deleteManifestationR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result
