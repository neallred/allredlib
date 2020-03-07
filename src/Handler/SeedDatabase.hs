{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SeedDatabase where

import Data.Aeson
import Import
import Genre
import Seed
import qualified Data.ByteString.Lazy as B

has :: [a] -> Bool
has = (> 0) . length

jsonFile :: FilePath
jsonFile = "seed/Genre.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

postSeedDatabaseR :: Handler Value
postSeedDatabaseR = do
  hasTitles <- fmap has $ runDB $ selectList [TitleTitle >. ""] []
  hasAuthors <- fmap has $ runDB $ selectList [CreatorFirstName >. ""] []
  hasManifestationPrintings <- fmap has $ runDB $ selectList [ManifestationPrintingVolumeNumber !=. 0] []
  hasManifestations <- fmap  has $ runDB $ selectList [PrintingVolumes !=. 0] []
  genres <- liftIO $ (eitherDecode <$> getJSON :: (IO (Either String [GenreEnum])))
  let needsSeeding = all id [hasTitles, hasAuthors, hasManifestationPrintings, hasManifestations]
  lordOfTheRingsId <- runDB $ insert $ lotr
  returnJson ("needsSeeding" :: Text, needsSeeding :: Bool, lordOfTheRingsId :: Key Title)
