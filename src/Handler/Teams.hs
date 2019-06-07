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

deleteFormClass :: String
deleteFormClass = "member-delete"

getTeamR :: TeamId -> Handler Html
getTeamR teamId = do
    (userId, user) <- requireAuthPair
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm teamMemberForm
    team <- runDB $ get404 teamId

    case result of
        FormSuccess formInput -> do
            let name = fromMaybe (userIdent $ entityVal $ teamMemberUser' formInput) (teamMemberName' formInput)
            let ent = TeamMember name (teamMemberDescription' formInput) (entityKey $ teamMemberUser' formInput) teamId
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    isMember <- isMemberOf userId teamId
    let canWrite = userAdmin user || isMember

    members <- runDB $ selectList [TeamMemberTeamId ==. teamId] []

    defaultLayout $ do
        $(widgetFile "team")

postTeamR :: TeamId -> Handler Html
postTeamR = getTeamR

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
        userOptions = optionsPersist [] [] userIdent

deleteTeamMemberR :: TeamMemberId -> Handler Value
deleteTeamMemberR memberId = do
    (userId, user) <- requireAuthPair
    member <- runDB $ get404 memberId
    let teamId = teamMemberTeamId member
    isMember <- isMemberOf userId teamId
    let canWrite = userAdmin user || isMember
    case canWrite of
        True -> do
            runDB $ delete memberId
            redirect (TeamR teamId)
        _ -> redirect (TeamR teamId)

