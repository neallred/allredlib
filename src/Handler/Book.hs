{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Book where

import Import
import Database.Persist.Postgresql

getBookR :: Handler Value
getBookR = do
  titles <- runDB $ selectList [TitleTitle >. ""] []
  -- editions <- runDB $ selectList [EditionEdition >. ""] []
  -- printings <- runDB $ selectList [PrintingPrinting >. ""] []
  -- manifestations <- runDB $ selectList [ManifestationCondition <. BookCondition.BookNew] []
  -- genres <- liftIO $ (eitherDecode <$> getJSON :: (IO (Either String [GenreEnum])))
  -- titles <- fmap has $ runDB $ selectList [TitleTitle >. ""] [Asc TitleTitle]

  $logInfo "info message"
  $logWarn "warn message"
  $logError "error message"
  let ids = (fmap (fromSqlKey . entityKey) titles)
  liftIO $ print $ ids
  returnJson $ zip ids titles
