{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Database.Persist.Sql (fromSqlKey)
import Import
import Text.Julius (RawJS (..))
import Yesod.Form.Bootstrap3
import Data.Text as DT
import Data.Maybe (fromMaybe)

data TeamForm = TeamForm
    { teamName' :: Text
    , teamDescription' :: Maybe Text
    , parent' :: Maybe (Entity Team)
    }

teamForm :: AForm Handler TeamForm
teamForm = TeamForm
    <$> areq textField (bfs ("Name" :: Text)) Nothing
    <*> aopt textField (bfs ("Description" :: Text)) Nothing
    <*> aopt (selectField teamOptions) (bfs ("Parent Team" :: Text)) Nothing
    where
        teamOptions = optionsPersist [] [Asc TeamName] teamName

getAdminR :: Handler Html
getAdminR = do
    (userId, user) <- requireAuthPair
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm teamForm

    case result of
        FormSuccess formInput -> do
            let ent = Team (teamName' formInput) (teamDescription' formInput) (entityKey <$> parent' formInput)
            runDB $ insertUnique ent
            return ()
        _ -> return ()

    allTeams <- runDB $ selectList [] [Asc TeamName]
    defaultLayout $ do
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = getAdminR
