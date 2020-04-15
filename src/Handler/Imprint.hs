{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Imprint where

import Import
import Data.Aeson.Types ( Result(..) )

getImprintsR :: Handler Value
getImprintsR = do
  imprints <- runDB $ selectList [] [] :: Handler [Entity Imprint]
  returnJson $ imprints

postImprintsR :: Handler Value
postImprintsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Imprint)

  returnJson insertResult

getImprintR :: ImprintId -> Handler Value
getImprintR imprintId = do
  imprint <- runDB $ selectFirst [ImprintId ==. imprintId] [] :: Handler (Maybe (Entity Imprint))
  returnJson $ imprint

putImprintR :: ImprintId -> Handler Value
putImprintR imprintId = do
  updatedImprint <- parseCheckJsonBody
  result <- case updatedImprint of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace imprintId x
  returnJson [result]

deleteImprintR :: ImprintId -> Handler Value
deleteImprintR imprintId = do
  result <- runDB $ delete imprintId
  returnJson result
