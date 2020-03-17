{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Series where

import Import
import Database.Persist.Postgresql
import Yesod.Form.Input

seriesIForm :: FormInput Handler Series
seriesIForm = Series
    <$> iopt textField "synopsis"
    <*> ireq textField "title"
    <*> ireq intField "books"
    <*> ireq intField "subseries"

getSeriesR :: Handler Value
getSeriesR = do
  series <- runDB $ selectList [SeriesTitle >. ""] []
  returnJson $ series

postSeriesR :: Handler Value
postSeriesR = do
  formResultSeries <- runInputPostResult seriesIForm
  insertResult <- case formResultSeries of
    FormFailure x -> sendStatusJSON badRequest400 x
    FormSuccess x -> do
      runDB $ insert $ x

  returnJson insertResult

putSeriesSingularR :: SeriesId -> Handler Value
putSeriesSingularR seriesId = do
  updatedSeries <- requireCheckJsonBody :: Handler Series
  result <- runDB $ replace seriesId $ updatedSeries
  returnJson [result]

deleteSeriesSingularR :: SeriesId -> Handler Value
deleteSeriesSingularR seriesId = do
  result <- runDB $ delete seriesId
  returnJson [result]


