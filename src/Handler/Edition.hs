{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Edition where

import Import
import Data.Aeson.Types ( Result(..) )

getEditionsR :: Handler Value
getEditionsR = do
  edition <- runDB $ selectList [] [] :: Handler [Entity Edition]
  returnJson $ edition

postEditionsR :: Handler Value
postEditionsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Edition)

  returnJson insertResult

getEditionR :: EditionId -> Handler Value
getEditionR editionId = do
  edition <- runDB $ selectFirst [EditionId ==. editionId] [] :: Handler (Maybe (Entity Edition))
  returnJson $ edition

putEditionR :: EditionId -> Handler Value
putEditionR editionId = do
  updatedSeries <- parseCheckJsonBody
  result <- case updatedSeries of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace editionId x
  returnJson [result]

deleteEditionR :: EditionId -> Handler Value
deleteEditionR editionId = do
  result <- runDB $ delete editionId
  returnJson result

