{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Seed where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Import
import BookBinding
import BookCondition
import Language
import System.Environment (getEnvironment)

data ImportedTitle = ImportedTitle
  String -- title id
  String -- title
  (Maybe Int) -- year
  (Maybe String) -- synopsis
  (Maybe String) -- seriesPart
  (Maybe String) -- seriesId
  (Maybe String) -- subseriesPart
  (Maybe String) -- subseriesId
  deriving (Generic, Show)

instance ToJSON ImportedTitle where
  toJSON (ImportedTitle id title year synopsis seriesPart seriesId subseriesPart subseriesId) =
    object
    [ "id" .= id
    , "title" .= title
    , "year" .= year
    , "synopsis" .= synopsis
    , "seriesPart" .= seriesPart
    , "seriesId" .= seriesId
    , "subseriesPart" .= subseriesPart
    , "subseriesId" .= subseriesId
    ]

instance FromJSON ImportedTitle where
  parseJSON (Object v) =
    ImportedTitle <$> v .: "id"
                  <*> v .: "title"
                  <*> v .: "year"
                  <*> v .: "synopsis"
                  <*> v .: "seriesPart"
                  <*> v .: "seriesId"
                  <*> v .: "subseriesPart"
                  <*> v .: "subseriesId"
  parseJSON _ = mzero

findWithDefault :: (a -> Bool) -> a -> [a] -> a
findWithDefault f z [] = z
findWithDefault f z (x:xs) =
  if f x then x else Seed.findWithDefault f z xs

runImporter :: IO ()
runImporter = do
  env <- getEnvironment
  let (_, seed_path) = Seed.findWithDefault (\(k, v) -> k == "ALLREDLIB_SEED_PATH") ("", "") env
  titlesStr <- B.readFile $ seed_path ++ "/Title.json"
  creators <- B.readFile $ seed_path ++ "/Creator.json"
  creatorTitles <- B.readFile $ seed_path ++ "/CreatorTitle.json"
  attributions <- B.readFile $ seed_path ++ "/Attribution.json"
  series <- B.readFile $ seed_path ++ "/Series.json"
  subseries <- B.readFile $ seed_path ++ "/Subseries.json"
  -- genreTitles genre ids match the ordering of enums, indexed starting with 1
  genreTitles <- B.readFile $ seed_path ++ "/GenreTitle.json"
  let eitherTitles = (eitherDecode titlesStr) :: Either String [ImportedTitle]
  let numTitles = case eitherTitles of
                    Left _ -> 0
                    Right titles -> length titles
  putStrLn "Need to seed:"
  putStrLn $ (pack . show $ numTitles) ++ " titles"
  pure ()

lotrSeries :: Import.Series
lotrSeries =
  Series
    (Just "The bestest fantasy series ever.")
    "The Lord of the Rings"
    3
    0

makeFellowship :: Key Import.Series -> Title
makeFellowship seriesId =
  Title
    (Just seriesId)
    (Just "1")
    Nothing
    Nothing
    (Just "Frodo gets the ring and goes on a journey.")
    "The Fellowship of the Ring"
    (Just 1954)

makeTwoTowers :: Key Import.Series -> Title
makeTwoTowers seriesId =
  Title
    (Just seriesId)
    (Just "2")
    Nothing
    Nothing
    (Just "Frodo journeys the hardest.")
    "The Two Towers"
    (Just 1954)

makeReturnOfTheKing :: Key Import.Series -> Title
makeReturnOfTheKing seriesId =
  Title
    (Just seriesId)
    (Just "3")
    Nothing
    Nothing
    (Just "Frodo's journey ends.")
    "The Return of the King"
    (Just 1955)

makeManifestation :: Manifestation
makeManifestation = Manifestation BookCondition.BookNew Nothing

makeManifestationPrinting :: Key Manifestation -> Key Printing -> ManifestationPrinting
makeManifestationPrinting mId pId = ManifestationPrinting mId pId Nothing Nothing

makeLotrEdition :: ImprintPublisherId -> TitleId -> Edition
makeLotrEdition imprintPublisherId titleId = 
  Edition 
    (Just BookBinding.PerfectBound)
    (Just Language.English)
    "Cheap paperback edition"
    imprintPublisherId
    Nothing
    titleId
    Nothing
    Nothing

makeLotrPrinting :: Key Edition -> Text -> Int -> Int -> Printing
makeLotrPrinting editionId isbn year pages = 
  Printing
    editionId
    (Just isbn)
    1
    (Just year)
    (Just pages)

torPublisher :: Publisher
torPublisher =
  Publisher
    "Tor Books"

torImprint :: Imprint
torImprint =
  Imprint
    "Tor Fantasy"

jrrTolkien :: Creator
jrrTolkien =
  Creator 
  (Just "J.R.R. Tolkien grew up in South Africa. He was an air warden in W.W.I.I.")
  (Just (fromGregorian 1892 1 3))
  (Just (fromGregorian 1973 9 2))
  "J.R.R"
  (Just "Tolkien")

makeImprintPublisher :: ImprintId -> PublisherId -> ImprintPublisher
makeImprintPublisher =
  ImprintPublisher 
