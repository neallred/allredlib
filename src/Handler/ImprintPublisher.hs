{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ImprintPublisher where

import Import
import Data.Aeson.Types ( Result(..) )

getImprintPublishersR :: Handler Value
getImprintPublishersR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity ImprintPublisher]
  returnJson $ queryResult

postImprintPublishersR :: Handler Value
postImprintPublishersR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: ImprintPublisher)

  returnJson insertResult

deleteImprintPublisherR :: ImprintPublisherId -> Handler Value
deleteImprintPublisherR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result
