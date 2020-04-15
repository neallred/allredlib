{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Publisher where

import Import
import Data.Aeson.Types ( Result(..) )

getPublishersR :: Handler Value
getPublishersR = do
  publishers <- runDB $ selectList [] [] :: Handler [Entity Publisher]
  returnJson $ publishers

postPublishersR :: Handler Value
postPublishersR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Publisher)

  returnJson insertResult

getPublisherR :: PublisherId -> Handler Value
getPublisherR publisherId = do
  publisher <- runDB $ selectFirst [PublisherId ==. publisherId] [] :: Handler (Maybe (Entity Publisher))
  returnJson $ publisher

putPublisherR :: PublisherId -> Handler Value
putPublisherR publisherId = do
  updatedPublisher <- parseCheckJsonBody
  result <- case updatedPublisher of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace publisherId x
  returnJson [result]

deletePublisherR :: PublisherId -> Handler Value
deletePublisherR publisherId = do
  result <- runDB $ delete publisherId
  returnJson result

