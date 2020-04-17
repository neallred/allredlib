{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.CreatorAttribution where

import Import
import Data.Aeson.Types ( Result(..) )

getCreatorAttributionsR :: Handler Value
getCreatorAttributionsR = do
  series <- runDB $ selectList [] [] :: Handler [Entity CreatorAttribution]
  returnJson $ series

postCreatorAttributionsR :: Handler Value
postCreatorAttributionsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: CreatorAttribution)

  returnJson insertResult

deleteCreatorAttributionR :: CreatorAttributionId -> Handler Value
deleteCreatorAttributionR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result
