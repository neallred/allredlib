{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Genre where

import Import
import Genre
-- import Data.Aeson.Types ( Result(..) )

getGenresR :: Handler Value
getGenresR = do
  -- creators <- runDB $ selectList [] [] :: Handler [Entity Genre]
  returnJson $ Genre.allGenres
