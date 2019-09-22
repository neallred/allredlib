{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module UserRole where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data UserRole = Anonymous
                   | Patron
                   | Admin
  
    deriving (Generic, Show, Read, Eq)
derivePersistField "UserRole"

instance ToJSON UserRole
instance FromJSON UserRole



