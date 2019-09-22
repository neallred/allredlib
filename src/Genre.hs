{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Genre where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data GenreEnum = Action
    | Biography
    | Children
    | Classic
    | Dystopia
    | Fantasy
    | HistoricalFiction
    | History
    | Horror
    | Junior
    | Mystery
    | Religion
    | Romance
    | ScienceFiction
    | Textbook
    | Travel
    | WorldLiterature
    | YoungAdult
  
    deriving (Generic, Show, Read, Eq)
derivePersistField "GenreEnum"

instance ToJSON GenreEnum
instance FromJSON GenreEnum
