{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SeedDatabase where

import Data.Aeson
import Import
import Seed
import BookCondition
import qualified Data.ByteString.Lazy as B

has :: [a] -> Bool
has = (> 0) . length

existy :: Maybe a -> Bool
existy (Just _) = True
existy _ = False

jsonFile :: FilePath
jsonFile = "seed/Genre.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

runSeeder :: HandlerFor App (Maybe ())
runSeeder = do
  lotrSeriesId <- (runDB $ insert lotrSeries)
  torImprintId <- (runDB $ insert torImprint)
  torPublisherId <- (runDB $ insert torPublisher)
  torImprintPublisherId <- (runDB $ insert $ makeImprintPublisher torImprintId torPublisherId)
  tolkienId <- (runDB $ insert jrrTolkien)

  fellowshipId <- runDB $ insert $ makeFellowship lotrSeriesId
  towersId <- runDB $ insert $ makeTwoTowers lotrSeriesId
  returnId <- runDB $ insert $ makeReturnOfTheKing lotrSeriesId

  fellowshipEditionId <- (runDB $ insert $ makeLotrEdition torImprintPublisherId fellowshipId)
  towersEditionId <- runDB $ insert $ makeLotrEdition torImprintPublisherId towersId
  returnEditionId <- runDB $ insert $ makeLotrEdition torImprintPublisherId returnId

  fellowshipPrintingId <- runDB $ insert $ makeLotrPrinting fellowshipEditionId "isbn-1" 1954 800
  towersPrintingId <- runDB $ insert $ makeLotrPrinting towersEditionId "isbn-2" 1954 804
  returnPrintingId <- runDB $ insert $ makeLotrPrinting returnEditionId "isbn-3" 1955 809

  fellowshipManifestationId <- runDB $ insert $ makeManifestation
  towersManifestationId <- runDB $ insert $ makeManifestation
  returnManifestationId <- runDB $ insert $ makeManifestation

  fellowshipManifestationPrintingId <- runDB $ insert $ makeManifestationPrinting fellowshipManifestationId fellowshipPrintingId
  towersManifestationPrintingId <- runDB $ insert $ makeManifestationPrinting towersManifestationId towersPrintingId
  returnManifestationPrintingId <- runDB $ insert $ makeManifestationPrinting returnManifestationId returnPrintingId

  return (Just ())

postSeedDatabaseR :: Handler Value
postSeedDatabaseR = do
  hasAuthors <- fmap existy $ runDB $ selectFirst [CreatorFirstName >. ""] []
  hasTitles <- fmap existy $ runDB $ selectFirst [TitleTitle >. ""] []
  hasEditions <- fmap existy $ runDB $ selectFirst [EditionName >. ""] []
  hasPrintings <- fmap existy $ runDB $ selectFirst [PrintingPrinting !=. 0] []
  hasManifestations <- fmap existy $ runDB $ selectFirst ([ManifestationCondition !=. BookCondition.BookNew] ||. [ManifestationCondition ==. BookCondition.BookNew]) []
  let needsSeeding = all not [hasAuthors, hasTitles, hasEditions, hasPrintings, hasManifestations]
  liftIO runImporter
  seedingReturnValue <- (if needsSeeding
                          then runSeeder
                          else return Nothing
                      )
  returnJson (("needsSeeding: " :: Text) ++ if needsSeeding then "true" else "false", "hasAuthors: " :: Text, show hasAuthors)
