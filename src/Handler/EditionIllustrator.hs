{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.EditionIllustrator where

import Import
import Data.Aeson.Types ( Result(..) )

getEditionIllustratorsR :: Handler Value
getEditionIllustratorsR = do
  queryResult <- runDB $ selectList [] [] :: Handler [Entity EditionIllustrator]
  returnJson $ queryResult

postEditionIllustratorsR :: Handler Value
postEditionIllustratorsR = do
  formResult <- parseCheckJsonBody
  insertResult <- case formResult of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: EditionIllustrator)

  returnJson insertResult

deleteEditionIllustratorR :: EditionIllustratorId -> Handler Value
deleteEditionIllustratorR resourceId = do
  result <- runDB $ delete resourceId
  returnJson result

