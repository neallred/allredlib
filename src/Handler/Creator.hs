{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Creator where

import Import
import Data.Aeson.Types ( Result(..) )

getCreatorsR :: Handler Value
getCreatorsR = do
  creators <- runDB $ selectList [] [] :: Handler [Entity Creator]
  returnJson $ creators

postCreatorsR :: Handler Value
postCreatorsR = do
  formResultCreator <- parseCheckJsonBody
  insertResult <- case formResultCreator of
    Error x -> sendStatusJSON badRequest400 x
    Success x -> runDB $ insert (x :: Creator)

  returnJson insertResult

getCreatorR :: CreatorId -> Handler Value
getCreatorR creatorId = do
  creator <- runDB $ selectFirst [CreatorId ==. creatorId] [] :: Handler (Maybe (Entity Creator))
  returnJson $ creator

putCreatorR :: CreatorId -> Handler Value
putCreatorR creatorId = do
  updatedCreator <- parseCheckJsonBody
  result <- case updatedCreator of
              Error x -> sendStatusJSON badRequest400 x
              Success x -> runDB $ replace creatorId x
  returnJson [result]

deleteCreatorR :: CreatorId -> Handler Value
deleteCreatorR creatorId = do
  result <- runDB $ delete creatorId
  returnJson result

