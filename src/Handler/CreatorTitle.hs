{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.CreatorTitle where

import Import
import Data.Aeson.Types ( Result(..) )

getCreatorTitlesR :: Handler Value
getCreatorTitlesR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity CreatorTitle]
  returnJson $ queryResult

postCreatorTitlesR :: Handler Value
postCreatorTitlesR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: CreatorTitle)

  returnJson insertResult

deleteCreatorTitleR :: CreatorTitleId -> Handler Value
deleteCreatorTitleR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result

