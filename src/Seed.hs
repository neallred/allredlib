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

makeFellowship seriesId =
  Title
    (Just seriesId)
    (Just 1)
    Nothing
    Nothing
    Nothing
    (Just "Frodo gets the ring and goes on a journey.")
    "The Fellowship of the Ring"
    (Just 1954)

makeTwoTowers seriesId =
  Title
    (Just seriesId)
    (Just 2)
    Nothing
    Nothing
    Nothing
    (Just "Frodo journeys the hardest.")
    "The Two Towers"
    (Just 1954)

makeReturnOfTheKing seriesId =
  Title
    (Just seriesId)
    (Just 3)
    Nothing
    Nothing
    Nothing
    (Just "Frodo's journey ends.")
    "The Return of the King"
    (Just 1955)

makeManifestation = Manifestation BookCondition.BookNew Nothing
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
