{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Database.Persist.Sql (fromSqlKey)
import Import
import Yesod.Form.Bootstrap3
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.List as L

data Tree = Empty | Tree Team [Tree]

teamsToTree :: [Entity Team] -> Entity Team -> Tree
teamsToTree teams t = Tree (entityVal t) $ map (teamsToTree teams) xs
    where
        xs = [a | a <- teams, (fromJust $ (teamParent . entityVal) a) == (entityKey t)]

deleteFormClass :: String
deleteFormClass = "team-delete"

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
    (_, _) <- requireAuthPair
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm teamForm

    case result of
        FormSuccess formInput -> do
            let ent = Team (teamName' formInput) (teamDescription' formInput) (entityKey <$> parent' formInput)
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    allTeams <- runDB $ selectList [] [Asc TeamName]
    let teamTree = L.groupBy (\ta tb -> teamParent ta == teamParent tb) $ map entityVal allTeams
    defaultLayout $ do
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = getAdminR

deleteAdminTeamR :: TeamId -> Handler Value
deleteAdminTeamR teamId = do
    team <- runDB $ get404 teamId
    runDB $ delete teamId
    redirect AdminR
