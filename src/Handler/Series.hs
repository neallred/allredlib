{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Series where

import Import
import Database.Persist.Postgresql
import Yesod.Form.Input
-- import Network.HTTP.Types

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

deleteSeriesR :: Handler Value
deleteSeriesR = do
  returnJson ["TBD"::Text]
