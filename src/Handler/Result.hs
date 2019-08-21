{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Result where

import Database.Persist.Sql (fromSqlKey)
import Import
import Data.Maybe (fromMaybe)
import Yesod.Form.Bootstrap3
import qualified Data.List as L
