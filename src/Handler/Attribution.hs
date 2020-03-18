{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Attribution where

import Import
import Data.Aeson.Types (Result(..))
import Network.URI

getAttributionsR :: Handler Value
getAttributionsR = do
  attributions <- runDB $ selectList [] [] :: Handler [Entity Attribution]
  returnJson $ attributions

postAttributionsR :: Handler Value
postAttributionsR = do
  attribution <- parseCheckJsonBody
  insertResult <- case attribution of
    Error x -> sendStatusJSON badRequest400 x
    Success x@(Attribution _ mLink _ _ _ _ _) -> do
      case mLink of
        Nothing ->
          runDB $ insert (x :: Attribution)
        Just lnk ->
          case parseURI $ unpack lnk of
            Nothing ->
              sendStatusJSON badRequest400 [("Citation link is invalid " :: Text) ++ "\"" ++ lnk ++ "\""]
            Just _ ->
              runDB $ insert (x :: Attribution)

  returnJson insertResult

putAttributionR :: AttributionId -> Handler Value
putAttributionR aId = do
  updatedAttribution <- requireCheckJsonBody :: Handler Attribution
  result <- runDB $ replace aId $ updatedAttribution
  returnJson [result]

deleteAttributionR :: AttributionId -> Handler Value
deleteAttributionR aId = do
  result <- runDB $ delete aId
  returnJson [result]
