{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.CheckIn where

import Import
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Yesod.Form.Bootstrap4

getCheckInR :: TeamId -> Handler Html
getCheckInR objectiveId = 
    defaultLayout $ do
        $(widgetFile "checkin")

postCheckInR :: TeamId -> Handler Html
postCheckInR = getCheckInR

deleteCheckInR :: TeamId -> Handler Html
deleteCheckInR = getCheckInR
