{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Book where

import Import
import Data.Aeson.Types ( Result(..) )
import qualified Database.Esqueleto as E

getBookR :: Handler Value
getBookR = do
  titleSeries <- runDB $ E.select $
    E.from $ \(t, s) -> do
    E.where_ (t E.^. TitleSeriesId E.==. s E.?. SeriesId)
    E.orderBy [E.asc (t E.^. TitleTitle)]
    return (t, s)

  returnJson titleSeries


getTitlesR :: Handler Value
getTitlesR = do
  titles <- runDB $ selectList [] [] :: Handler [Entity Title]
  returnJson titles

postTitlesR :: Handler Value
postTitlesR = do
  formResultSeries <- parseCheckJsonBody
  insertResult <- case formResultSeries of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Title)

  returnJson insertResult

getTitleR :: TitleId -> Handler Value
getTitleR titleId = do
  title <- runDB $ selectFirst [TitleId ==. titleId] [] :: Handler (Maybe (Entity Title))
  returnJson $ title

putTitleR :: TitleId -> Handler Value
putTitleR titleId = do
  updatedSeries <- parseCheckJsonBody
  result <- case updatedSeries of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace titleId x
  returnJson [result]

deleteTitleR :: TitleId -> Handler Value
deleteTitleR titleId = do
  result <- runDB $ delete titleId
  returnJson result
