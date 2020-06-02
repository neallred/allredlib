{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Seed where

import Import
import BookBinding
import BookCondition
import Language

lotrSeries :: Series
lotrSeries =
  Series
    (Just "The bestest fantasy series ever.")
    "The Lord of the Rings"
    3
    0

makeFellowship :: Key Series -> Title
makeFellowship seriesId =
  Title
    (Just seriesId)
    (Just "1")
    Nothing
    Nothing
    (Just "Frodo gets the ring and goes on a journey.")
    "The Fellowship of the Ring"
    (Just 1954)

makeTwoTowers :: Key Series -> Title
makeTwoTowers seriesId =
  Title
    (Just seriesId)
    (Just "2")
    Nothing
    Nothing
    (Just "Frodo journeys the hardest.")
    "The Two Towers"
    (Just 1954)

makeReturnOfTheKing :: Key Series -> Title
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
