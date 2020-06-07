{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Seeder where

import Import
import Data.Aeson
import Data.Either (fromRight)
import qualified Data.ByteString.Lazy as B
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import qualified Data.Map as Map
import Genre

import System.Environment (getEnvironment)

data SeedTitle = SeedTitle
  Text -- title id
  (Maybe Text) -- seriesId
  (Maybe Text) -- seriesPart
  (Maybe Text) -- subseriesId
  (Maybe Text) -- subseriesPart
  (Maybe Text) -- synopsis
  Text -- title
  (Maybe Int) -- year
  deriving (Show)

toTitle :: SeedTitle -> Map Text (Key Import.Series) -> Map Text (Key Subseries) -> Title
toTitle (SeedTitle _ seriesId seriesPart subseriesId subseriesPart synopsis title year) mapSeriesIds mapSubseriesIds =
  Title (Map.lookup (fromMaybe "NO MATCH" seriesId) mapSeriesIds) seriesPart (Map.lookup (fromMaybe "NO MATCH" subseriesId) mapSubseriesIds) subseriesPart synopsis title year

getTitleId :: SeedTitle -> Text
getTitleId (SeedTitle seedId _ _ _ _ _ _ _) = seedId

instance ToJSON SeedTitle where
  toJSON (SeedTitle seedId title year synopsis seriesPart seriesId subseriesPart subseriesId) =
    object
    [ "id" .= seedId
    , "seriesId" .= seriesId
    , "seriesPart" .= seriesPart
    , "subseriesId" .= subseriesId
    , "subseriesPart" .= subseriesPart
    , "synopsis" .= synopsis
    , "title" .= title
    , "year" .= year
    ]

instance FromJSON SeedTitle where
  parseJSON (Object v) =
    SeedTitle <$> v .: "id"
                  <*> v .: "seriesId"
                  <*> v .: "seriesPart"
                  <*> v .: "subseriesId"
                  <*> v .: "subseriesPart"
                  <*> v .: "synopsis"
                  <*> v .: "title"
                  <*> v .: "year"
  parseJSON _ = mzero

data SeedCreator = SeedCreator
  Text -- id
  (Maybe Text) -- bio
  (Maybe Int) -- birth
  (Maybe Int) -- death
  Text -- firstName
  (Maybe Text) -- lastName
  deriving (Show)

toCreator :: SeedCreator -> Creator
toCreator (SeedCreator _ bio birth death firstName lastName) = Creator bio birth death firstName lastName

getCreatorId :: SeedCreator -> Text
getCreatorId (SeedCreator seedId _ _ _ _ _) = seedId

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

getSeriesId :: SeedSeries -> Text
getSeriesId (SeedSeries seedId _ _ _ _ _) = seedId

toSeries :: SeedSeries -> Import.Series
toSeries (SeedSeries _ synopsis title totalBookMembers totalSubseries _) =
  Series synopsis title totalBookMembers totalSubseries

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

getSubseriesId :: SeedSubseries -> Text
getSubseriesId (SeedSubseries seedId _ _ _ _ _) = seedId

toSubseries :: SeedSubseries -> Map Text (Key Import.Series) -> Subseries
toSubseries (SeedSubseries _ seedSeriesId synopsis title totalBookMembers _) map =
  Subseries (map Map.! seedSeriesId) synopsis title totalBookMembers

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

countList :: Either String [a] -> Text -> Text
countList xs label = ((pack . show . length) (fromRight [] xs)) ++ " " ++ label

textToGenreId :: Text -> Map GenreEnum (Key Genre) -> Key Genre
textToGenreId textId map =
  let genreEnum = case textId of
                    "1" -> Action
                    "2" -> Biography
                    "3" -> Children
                    "4" -> Classic
                    "5" -> Dystopia
                    "6" -> Fantasy
                    "7" -> HistoricalFiction
                    "8" -> History
                    "9" -> Horror
                    "10" -> Junior
                    "11" -> Mystery
                    "12" -> Religion
                    "13" -> Romance
                    "14" -> ScienceFiction
                    "15" -> Textbook
                    "16" -> Travel
                    "17" -> WorldLiterature
                    "18" -> YoungAdult
                    "_" -> undefined
  in map Map.! genreEnum

mkMap :: (Ord a) => [a] -> [b] -> Map a b
mkMap xs ys = Map.fromList $ zip xs ys

runImporter :: IO ()
runImporter = do
  let titlesStr = fromStrict $(makeRelativeToProject "seed/Title.json" >>= embedFile)
  let creatorsStr = fromStrict $(makeRelativeToProject "seed/Creator.json" >>= embedFile)
  let creatorTitlesStr = fromStrict $(makeRelativeToProject "seed/CreatorTitle.json" >>= embedFile)
  let attributionsStr = fromStrict $(makeRelativeToProject "seed/Attribution.json" >>= embedFile)
  let seriesStr = fromStrict $(makeRelativeToProject "seed/Series.json" >>= embedFile)
  let subseriesStr = fromStrict $(makeRelativeToProject "seed/Subseries.json" >>= embedFile)
  -- genreTitles genre ids match the ordering of enums, indexed starting with 1
  let genreTitlesStr = fromStrict $(makeRelativeToProject "seed/GenreTitle.json" >>= embedFile)
  let eitherTitles = (eitherDecode titlesStr) :: Either String [SeedTitle]
  let eitherCreators = (eitherDecode creatorsStr) :: Either String [SeedCreator]
  let eitherCreatorTitles = (eitherDecode creatorTitlesStr) :: Either String [SeedCreatorTitle]
  let eitherAttributions = (eitherDecode attributionsStr) :: Either String [SeedAttribution]
  let eitherSeries = (eitherDecode seriesStr) :: Either String [SeedSeries]
  let eitherSubseries = (eitherDecode subseriesStr) :: Either String [SeedSubseries]
  let eitherGenreTitles = (eitherDecode genreTitlesStr) :: Either String [SeedGenreTitle]
  putStrLn "Need to seed:"
  putStrLn $ countList eitherTitles "titles"
  putStrLn $ countList eitherCreators "creators"
  putStrLn $ countList eitherCreatorTitles "creatorTitles"
  putStrLn $ countList eitherAttributions "attributions"
  putStrLn $ countList eitherSeries "series"
  putStrLn $ countList eitherSubseries "subseries"
  putStrLn $ countList eitherGenreTitles "genreTitles"

  env <- getEnvironment
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    let series = (fromRight [] eitherSeries)
    seriesIds <- mapM (insert . toSeries) series
    let subseries = (fromRight [] eitherSubseries)
    let seriesIdsMap = mkMap (map getSeriesId series) seriesIds 
    subseriesIds <- mapM (\seedSubseries -> insert $ toSubseries seedSubseries seriesIdsMap) subseries
    let subseriesIdsMap = mkMap (map getSubseriesId subseries) subseriesIds 
    let titles = (fromRight [] eitherTitles)
    titleIds <- mapM (\title -> insert $ toTitle title seriesIdsMap subseriesIdsMap) titles
    let titleIdsMap = mkMap (map getTitleId titles) titleIds
    genreIds <- mapM (\x -> insert $ Genre x) Genre.allGenres
    let genreIdsMap = mkMap Genre.allGenres genreIds
    let genreTitles = fromRight [] eitherGenreTitles 
    genreTitleIds <- mapM (\(SeedGenreTitle seedGenreId seedTitleId) -> insert $ GenreTitle (textToGenreId seedGenreId genreIdsMap) (titleIdsMap Map.! seedTitleId)) genreTitles
    let creators = fromRight [] eitherCreators
    creatorIds <- mapM (insert . toCreator) creators
    let creatorIdsMap = mkMap (map getCreatorId creators) creatorIds
    let creatorTitles = fromRight [] eitherCreatorTitles
    creatorTitleIds <- mapM (\(SeedCreatorTitle creatorId titleId) -> insert $ CreatorTitle (creatorIdsMap Map.! creatorId) (titleIdsMap Map.! titleId)) creatorTitles
    pure ()

  pure ()
