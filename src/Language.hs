{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Language where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data Language = Arabic
              | Aramaic
              | Armenian
              | Basque
              | Cantonese
              | Catalan
              | Coptic
              | Danish
              | Deseret
              | Dutch
              | English
              | Esperanto
              | Finnish
              | French
              | German
              | Greek
              | Hebrew
              | Hindi
              | Italian
              | Japanese
              | Latin
              | Mandarin
              | Russian
              | Tagalog
  
    deriving (Generic, Show, Read, Eq)
derivePersistField "Language"

instance ToJSON Language
instance FromJSON Language

