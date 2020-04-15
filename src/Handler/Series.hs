{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Series where

import Import
import Data.Aeson.Types ( Result(..) )

getSeriesR :: Handler Value
getSeriesR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity Series]
  returnJson $ queryResult

postSeriesR :: Handler Value
postSeriesR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Series)

  returnJson insertResult

getSeriesSingularR :: SeriesId -> Handler Value
getSeriesSingularR seriesId = do
  queryResult <- runDB $ selectFirst [SeriesId ==. seriesId] [] :: Handler (Maybe (Entity Series))
  returnJson $ queryResult

putSeriesSingularR :: SeriesId -> Handler Value
putSeriesSingularR seriesId = do
  updatedSeries <- parseCheckJsonBody
  result <- case updatedSeries of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace seriesId x
  returnJson [result]

deleteSeriesSingularR :: SeriesId -> Handler Value
deleteSeriesSingularR seriesId = do
  result <- runDB $ delete seriesId
  returnJson result
