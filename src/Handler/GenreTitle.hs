{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.GenreTitle where

import Import
import Data.Aeson.Types ( Result(..) )

getGenreTitlesR :: Handler Value
getGenreTitlesR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity GenreTitle]
  returnJson $ queryResult

postGenreTitlesR :: Handler Value
postGenreTitlesR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: GenreTitle)

  returnJson insertResult

deleteGenreTitleR :: GenreTitleId -> Handler Value
deleteGenreTitleR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result
