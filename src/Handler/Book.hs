{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Book where

import Import
import Data.Aeson.Types ( Result(..) )
import Database.Persist.Postgresql

getBookR :: Handler Value
getBookR = do
  titles <- runDB $ selectList [] [] :: Handler [Entity Title]
  -- editions <- runDB $ selectList [EditionEdition >. ""] []
  -- printings <- runDB $ selectList [PrintingPrinting >. ""] []
  -- manifestations <- runDB $ selectList [ManifestationCondition <. BookCondition.BookNew] []
  -- genres <- liftIO $ (eitherDecode <$> getJSON :: (IO (Either String [GenreEnum])))
  -- titles <- fmap has $ runDB $ selectList [TitleTitle >. ""] [Asc TitleTitle]

  $logInfo "info message"
  $logWarn "warn message"
  $logError "error message"
  let ids = (fmap (fromSqlKey . entityKey) titles)
  liftIO $ print $ ids
  returnJson $ zip ids titles


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
