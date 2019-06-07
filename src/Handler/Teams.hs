{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Teams where

import Database.Persist.Sql (fromSqlKey)
import Import
import Data.Maybe (fromMaybe)
import Yesod.Form.Bootstrap3
import qualified Data.List as L

getTeamsR :: Handler Value
getTeamsR = do
    allTeams <- runDB $ selectList [] [Asc TeamId]
    returnJson allTeams

postTeamsR :: Handler Value
postTeamsR = do
    team <- (requireJsonBody :: Handler Team)
    inserted <- runDB $ insertEntity team
    returnJson inserted

getTeamR :: TeamId -> Handler Html
getTeamR teamId = do
    (userId, user) <- requireAuthPair
    team <- runDB $ get404 teamId

    isMember <- isMemberOf userId teamId
    let canWrite = userAdmin user && isMember

    members <- runDB $ selectList [TeamMemberTeamId ==. teamId] []

    defaultLayout $ do
        $(widgetFile "team")

getTeamObjectivesR :: TeamId -> Handler Value
getTeamObjectivesR teamId = do
    objectives <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    returnJson (objectives :: [Entity Objective])

postTeamObjectivesR :: TeamId -> Handler Value
postTeamObjectivesR team = do
    objective <- (requireJsonBody :: Handler Objective)
    objectiveId <- runDB $ insert objective
    runDB $ update objectiveId [ObjectiveTeamId =. team]
    returnJson objective

isMemberOf :: UserId  -> TeamId -> Handler Bool
isMemberOf userId teamId = do
    members <- runDB $ selectList [TeamMemberUserId ==. userId, TeamMemberTeamId ==. teamId] []
    return $ not $ null members

data TeamMemberForm = TeamMemberForm
    { teamMemberName' :: Maybe Text
    , teamMemberDescription' :: Maybe Text
    , teamMemberUser' :: Entity User
    }

teamMemberForm :: AForm Handler TeamMemberForm
teamMemberForm = TeamMemberForm
    <$> aopt textField (bfs ("Name" :: Text)) Nothing
    <*> aopt textField (bfs ("Description" :: Text)) Nothing
    <*> areq (selectField userOptions) (bfs ("User" :: Text)) Nothing
    where
        userOptions = optionsPersist [] [Asc UserIdent] userIdent
