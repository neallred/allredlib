{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Seed where

import Import

makeSkeletonTitle :: Text -> Title
makeSkeletonTitle title =
  Title
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    title
    Nothing

lotr :: Title
lotr = makeSkeletonTitle "Lord of the Rings"
