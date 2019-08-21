{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Team where

import Database.Persist.Sql (fromSqlKey)
import Import
import Data.Maybe (fromMaybe)
import Yesod.Form.Bootstrap3
import qualified Data.List as L

getTeamR :: TeamId -> Handler Html
getTeamR teamId = do
    (userId, user) <- requireAuthPair
    (memberFormWidget, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm teamMemberForm
    (objectiveFormWidget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm addObjectiveForm
    team <- runDB $ get404 teamId
    objectives' <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    let objectives = entityVal <$> objectives'

    isMember <- isMemberOf userId teamId
    let isAdmin = userAdmin user || isMember

    members <- runDB $ selectList [TeamMemberTeamId ==. teamId] []

    defaultLayout $ do
        $(widgetFile "team")

postTeamR :: TeamId -> Handler Html
postTeamR teamId = do
    (userId, user) <- requireAuthPair
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm teamMemberForm

    case result of
        FormSuccess formInput -> do
            let name = fromMaybe (userIdent $ entityVal $ teamMemberUser' formInput) (teamMemberName' formInput)
            let ent = TeamMember name (teamMemberDescription' formInput) (entityKey $ teamMemberUser' formInput) teamId
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    redirect $ TeamR teamId

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

getTeamObjectivesR :: TeamId -> Handler Value
getTeamObjectivesR teamId = do
    objectives <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    returnJson (objectives :: [Entity Objective])

postTeamObjectivesR :: TeamId -> Handler Html
postTeamObjectivesR teamId = do
    (userId, user) <- requireAuthPair
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm addObjectiveForm

    case result of
        FormSuccess formInput -> do
            let ent = Objective (objectiveName' formInput) (objectiveDescription' formInput) teamId
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    redirect $ TeamR teamId


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

data AddObjectiveForm = AddObjectiveForm
    { objectiveName' :: Text
    , objectiveDescription' :: Text
    }

addObjectiveForm :: AForm Handler AddObjectiveForm
addObjectiveForm = AddObjectiveForm
    <$> areq textField (bfs ("Name" :: Text)) Nothing
    <*> areq textField (bfs ("Description" :: Text)) Nothing
