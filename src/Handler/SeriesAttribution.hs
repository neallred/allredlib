{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SeriesAttribution where

import Import

getSeriesAttributionsR :: Handler Value
getSeriesAttributionsR = do
  series <- runDB $ selectList [] [] :: Handler [Entity SeriesAttribution]
  returnJson $ series

postSeriesAttributionR :: SeriesId -> AttributionId -> Handler Value
postSeriesAttributionR seriesId attributionId = do
  insertResult <- runDB $ insert (SeriesAttribution seriesId attributionId)

  returnJson insertResult

deleteSeriesAttributionSingleR :: SeriesAttributionId -> Handler Value
deleteSeriesAttributionSingleR seriesId = do
  result <- runDB $ delete seriesId
  returnJson result
