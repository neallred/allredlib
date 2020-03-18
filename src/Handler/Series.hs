{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Series where

import Import
import Data.Aeson.Types ( Result(..) )

getSeriesR :: Handler Value
getSeriesR = do
  series <- runDB $ selectList [SeriesTitle >. ""] []
  returnJson $ series

postSeriesR :: Handler Value
postSeriesR = do
  formResultSeries <- parseCheckJsonBody
  insertResult <- case formResultSeries of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Series)

  returnJson insertResult

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
