{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Subseries where

import Import
import Data.Aeson.Types ( Result(..) )

getSubseriesR :: Handler Value
getSubseriesR = do
  series <- runDB $ selectList [] [] :: Handler [Entity Series]
  returnJson $ series

postSubseriesR :: Handler Value
postSubseriesR = do
  formResultSeries <- parseCheckJsonBody
  insertResult <- case formResultSeries of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Subseries)

  returnJson insertResult

getSubseriesSingularR :: SubseriesId -> Handler Value
getSubseriesSingularR subseriesId = do
  subseries <- runDB $ selectFirst [SubseriesId ==. subseriesId] [] :: Handler (Maybe (Entity Subseries))
  returnJson $ subseries

putSubseriesSingularR :: SubseriesId -> Handler Value
putSubseriesSingularR subseriesId = do
  updatedSeries <- parseCheckJsonBody
  result <- case updatedSeries of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace subseriesId x
  returnJson [result]

deleteSubseriesSingularR :: SubseriesId -> Handler Value
deleteSubseriesSingularR subseriesId = do
  result <- runDB $ delete subseriesId
  returnJson result

