{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Seeder where

import Import
import Data.Aeson
import Data.Either (fromRight)
import qualified Data.ByteString.Lazy as B

import System.Environment (getEnvironment)

data SeedTitle = SeedTitle
  Text -- title id
  Text -- title
  (Maybe Int) -- year
  (Maybe Text) -- synopsis
  (Maybe Text) -- seriesPart
  (Maybe Text) -- seriesId
  (Maybe Text) -- subseriesPart
  (Maybe Text) -- subseriesId
  deriving (Show)

instance ToJSON SeedTitle where
  toJSON (SeedTitle seedId title year synopsis seriesPart seriesId subseriesPart subseriesId) =
    object
    [ "id" .= seedId
    , "title" .= title
    , "year" .= year
    , "synopsis" .= synopsis
    , "seriesPart" .= seriesPart
    , "seriesId" .= seriesId
    , "subseriesPart" .= subseriesPart
    , "subseriesId" .= subseriesId
    ]

instance FromJSON SeedTitle where
  parseJSON (Object v) =
    SeedTitle <$> v .: "id"
                  <*> v .: "title"
                  <*> v .: "year"
                  <*> v .: "synopsis"
                  <*> v .: "seriesPart"
                  <*> v .: "seriesId"
                  <*> v .: "subseriesPart"
                  <*> v .: "subseriesId"
  parseJSON _ = mzero

data SeedCreator = SeedCreator
  Text -- id
  (Maybe Text) -- bio
  (Maybe Int) -- birth
  (Maybe Int) -- death
  Text -- firstName
  (Maybe Text) -- lastName
  deriving (Show)

instance FromJSON SeedCreator where
  parseJSON (Object v) =
    SeedCreator <$> v .: "id"
                    <*> v .: "bio"
                    <*> v .: "birth"
                    <*> v .: "death"
                    <*> v .: "firstName"
                    <*> v .: "lastName"
  parseJSON _ = mzero

instance ToJSON SeedCreator where
  toJSON (SeedCreator seedId bio birth death firstName lastName) =
    object
    [ "id" .= seedId
    , "bio" .= bio
    , "birth" .= birth
    , "death" .= death
    , "firstName" .= firstName
    , "lastName" .= lastName
    ]

data SeedCreatorTitle = SeedCreatorTitle
  Text -- creatorId
  Text -- titleId
  deriving (Show)

instance FromJSON SeedCreatorTitle where
  parseJSON (Object v) =
    SeedCreatorTitle <$> v .: "creatorId"
                    <*> v .: "titleId"
  parseJSON _ = mzero

instance ToJSON SeedCreatorTitle where
  toJSON (SeedCreatorTitle creatorId titleId) =
    object
    [ "creatorId" .= creatorId
    , "titleId" .= titleId
    ]

data SeedGenreTitle = SeedGenreTitle
  Text -- genreId
  Text -- titleId
  deriving (Show)

instance FromJSON SeedGenreTitle where
  parseJSON (Object v) =
    SeedGenreTitle <$> v .: "genreId"
                   <*> v .: "titleId"
  parseJSON _ = mzero

instance ToJSON SeedGenreTitle where
  toJSON (SeedGenreTitle genreId titleId) =
    object
    [ "genreId" .= genreId
    , "titleId" .= titleId
    ]

data SeedSeries = SeedSeries
  Text -- id
  (Maybe Text) -- synopsis
  Text -- title
  Int -- totalBookMembers
  Int -- totalSubseries
  [Text] -- attribution ids
  deriving (Show)

instance FromJSON SeedSeries where
  parseJSON (Object v) =
    SeedSeries <$> v .: "id"
                   <*> v .: "synopsis"
                   <*> v .: "title"
                   <*> v .: "totalBookMembers"
                   <*> v .: "totalSubseries"
                   <*> v .: "attributionIds"
  parseJSON _ = mzero

instance ToJSON SeedSeries where
  toJSON (SeedSeries seedId synopsis title totalBookMembers totalSubseries attributionIds) =
    object
    [ "id" .= seedId
    , "synopsis" .= synopsis
    , "title" .= title
    , "totalBookMembers" .= totalBookMembers
    , "totalSubseries" .= totalSubseries
    , "attributionIds" .= attributionIds
    ]

data SeedSubseries = SeedSubseries
  Text -- id
  Text -- seriesId
  (Maybe Text) -- synopsis
  Text -- title
  Int -- totalBookMembers
  [Text] -- attribution ids
  deriving (Show)

instance FromJSON SeedSubseries where
  parseJSON (Object v) =
    SeedSubseries <$> v .: "id"
                   <*> v .: "seriesId"
                   <*> v .: "synopsis"
                   <*> v .: "title"
                   <*> v .: "totalBookMembers"
                   <*> v .: "attributionIds"
  parseJSON _ = mzero

instance ToJSON SeedSubseries where
  toJSON (SeedSubseries seedId seriesId synopsis title totalBookMembers attributionIds) =
    object
    [ "id" .= seedId
    , "seriesId" .= seriesId
    , "synopsis" .= synopsis
    , "title" .= title
    , "totalBookMembers" .= totalBookMembers
    , "attributionIds" .= attributionIds
    ]

data SeedAttribution = SeedAttribution
  Text -- id
  Text -- title
  (Maybe Text) -- link
  (Maybe UTCTime) -- accessed
  (Maybe UTCTime) -- yearCreated
  (Maybe Text) -- placePublished
  (Maybe Text) -- author
  (Maybe Text) -- publisher
  deriving (Show)

instance FromJSON SeedAttribution where
  parseJSON (Object v) =
    SeedAttribution <$> v .: "id"
                    <*> v .: "title"
                    <*> v .: "link"
                    <*> v .: "accessed"
                    <*> v .: "yearCreated"
                    <*> v .: "placePublished"
                    <*> v .: "author"
                    <*> v .: "publisher"
  parseJSON _ = mzero

instance ToJSON SeedAttribution where
  toJSON (SeedAttribution seedId title seedLink accessed yearCreated placePublished author publisher) =
    object
    [ "id" .= seedId
    , "title" .= title
    , "link" .= seedLink
    , "accessed" .= accessed
    , "yearCreated" .= yearCreated
    , "placePublished" .= placePublished
    , "author" .= author
    , "publisher" .= publisher
    ]

findWithDefault :: (a -> Bool) -> a -> [a] -> a
findWithDefault _ z [] = z
findWithDefault f z (x:xs) =
  if f x then x else Seeder.findWithDefault f z xs

countList :: [a] -> Text
countList xs = (pack . show . length) xs

runImporter :: IO ()
runImporter = do
  env <- getEnvironment
  let (_, seed_path) = Seeder.findWithDefault (\(k, _) -> k == "ALLREDLIB_SEED_PATH") ("", "") env
  titlesStr <- B.readFile $ seed_path ++ "/Title.json"
  creatorsStr <- B.readFile $ seed_path ++ "/Creator.json"
  creatorTitlesStr <- B.readFile $ seed_path ++ "/CreatorTitle.json"
  attributionsStr <- B.readFile $ seed_path ++ "/Attribution.json"
  seriesStr <- B.readFile $ seed_path ++ "/Series.json"
  subseriesStr <- B.readFile $ seed_path ++ "/Subseries.json"
  -- genreTitles genre ids match the ordering of enums, indexed starting with 1
  genreTitlesStr <- B.readFile $ seed_path ++ "/GenreTitle.json"
  let eitherTitles = (eitherDecode titlesStr) :: Either String [SeedTitle]
  let eitherCreators = (eitherDecode creatorsStr) :: Either String [SeedCreator]
  let eitherCreatorTitles = (eitherDecode creatorTitlesStr) :: Either String [SeedCreatorTitle]
  let eitherAttributions = (eitherDecode attributionsStr) :: Either String [SeedAttribution]
  let eitherSeries = (eitherDecode seriesStr) :: Either String [SeedSeries]
  let eitherSubseries = (eitherDecode subseriesStr) :: Either String [SeedSubseries]
  let eitherGenreTitles = (eitherDecode genreTitlesStr) :: Either String [SeedGenreTitle]
  putStrLn "Need to seed:"
  putStrLn $ (countList $ fromRight [] eitherTitles) ++ " titles"
  putStrLn $ (countList $ fromRight [] eitherCreators) ++ " creators"
  putStrLn $ (countList $ fromRight [] eitherCreatorTitles) ++ " creatorTitles"
  putStrLn $ (countList $ fromRight [] eitherAttributions) ++ " attributions"
  putStrLn $ (countList $ fromRight [] eitherSeries) ++ " series"
  putStrLn $ (countList $ fromRight [] eitherSubseries) ++ " subseries"
  putStrLn $ (countList $ fromRight [] eitherGenreTitles) ++ " genreTitles"
  pure ()
