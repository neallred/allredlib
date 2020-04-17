{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Checkout where

import Import
import Data.Aeson.Types ( Result(..) )

getCheckoutsR :: Handler Value
getCheckoutsR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity Checkout]
  returnJson $ queryResult

postCheckoutsR :: Handler Value
postCheckoutsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Checkout)

  returnJson insertResult

getCheckoutR :: CheckoutId -> Handler Value
getCheckoutR resourceId = do
  queryResult <- runDB $ selectFirst [CheckoutId ==. resourceId] [] :: Handler (Maybe (Entity Checkout))
  returnJson $ queryResult

putCheckoutR :: CheckoutId -> Handler Value
putCheckoutR resourceId = do
  updatedResource <- parseCheckJsonBody
  result <- case updatedResource of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace resourceId x
  returnJson [result]

deleteCheckoutR :: CheckoutId -> Handler Value
deleteCheckoutR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result


