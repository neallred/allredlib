{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SubseriesAttribution where

import Import
import Data.Aeson.Types ( Result(..) )

getSubseriesAttributionsR :: Handler Value
getSubseriesAttributionsR = do
  series <- runDB $ selectList [] [] :: Handler [Entity SubseriesAttribution]
  returnJson $ series

postSubseriesAttributionsR :: Handler Value
postSubseriesAttributionsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: SubseriesAttribution)

  returnJson insertResult

deleteSubseriesAttributionR :: SubseriesAttributionId -> Handler Value
deleteSubseriesAttributionR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result

