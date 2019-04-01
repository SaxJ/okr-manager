{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Data.Maybe (fromJust)
import Database.Persist.Sql (fromSqlKey)

getAdminR :: Handler Html
getAdminR = do
    (userId, user) <- requireAuthPair

    defaultLayout $ do
        $(widgetFile "admin")
