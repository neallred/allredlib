{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Printing where

import Import
import Data.Aeson.Types ( Result(..) )

getPrintingsR :: Handler Value
getPrintingsR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity Printing]
  returnJson $ queryResult

postPrintingsR :: Handler Value
postPrintingsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Printing)

  returnJson insertResult

getPrintingR :: PrintingId -> Handler Value
getPrintingR printingId = do
  queryResult <- runDB $ selectFirst [PrintingId ==. printingId] [] :: Handler (Maybe (Entity Printing))
  returnJson $ queryResult

putPrintingR :: PrintingId -> Handler Value
putPrintingR printingId = do
  updatedResource <- parseCheckJsonBody
  result <- case updatedResource of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace printingId x
  returnJson [result]

deletePrintingR :: PrintingId -> Handler Value
deletePrintingR printingId = do
  result <- runDB $ delete printingId
  returnJson result

