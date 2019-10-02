{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SeedDatabase where

import Import

-- has = ((> 0) . length)

postSeedDatabaseR :: Handler Value
postSeedDatabaseR = do
  hasTitles <- fmap ((> 0) . length) $ runDB $ selectList [TitleTitle >. ""] []
  hasAuthors <- fmap ((> 0) . length) $ runDB $ selectList [CreatorLastName >. ""] []
  hasManifestationPrintings <- fmap ((> 0) . length) $ runDB $ selectList [ManifestationPrintingVolumeNumber !=. 0] []
  hasManifestations <- fmap  ((> 0) . length) $ runDB $ selectList [PrintingVolumes !=. 0] []
  let needsSeeding = all id [hasTitles, hasAuthors, hasManifestationPrintings, hasManifestations]
  -- print "manifestations"
  -- print hasManifestations
  returnJson ("needsSeeding" :: Text, needsSeeding :: Bool)
