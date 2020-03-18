{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "Jenna Allred Private Library"
    $(widgetFile "homepage")
