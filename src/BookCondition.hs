{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module BookCondition where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data BookCondition = New
                   | LikeNew
                   | Good
                   | Used
                   | VeryUsed
                   | Replace
  
    deriving (Generic, Show, Read, Eq)
derivePersistField "BookCondition"

instance ToJSON BookCondition
instance FromJSON BookCondition


