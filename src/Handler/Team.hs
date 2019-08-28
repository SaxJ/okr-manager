{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Team where

import Import
import Data.Maybe (fromMaybe)
import Yesod.Form.Bootstrap3
import qualified Handler.Objectives as OH

getTeamR :: TeamId -> Handler Html
getTeamR teamId = do
    (userId, user) <- requireAuthPair

    (memberFormWidget, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm teamMemberForm
    (objectiveFormWidget, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm addObjectiveForm
    (addResultFormWidget, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm OH.addResultForm

    team <- runDB $ get404 teamId
    objectives <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    results <- runDB $ selectList [ResultObjectiveId <-. ((\o -> entityKey o) <$> objectives)] [Asc ResultId]

    let groupResults obj = [r | r <- results, (resultObjectiveId $ entityVal r) == (entityKey obj)]

    isMember <- isMemberOf userId teamId
    let isAdmin = userAdmin user || isMember

    members <- runDB $ selectList [TeamMemberTeamId ==. teamId] []

    defaultLayout $ do
        $(widgetFile "team")

postTeamR :: TeamId -> Handler Html
postTeamR teamId = do
    _ <- requireAuthPair
    ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm teamMemberForm

    case result of
        FormSuccess formInput -> do
            let name = fromMaybe (userIdent $ entityVal $ teamMemberUser' formInput) (teamMemberName' formInput)
            let ent = TeamMember name (teamMemberDescription' formInput) (entityKey $ teamMemberUser' formInput) teamId
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    redirect $ TeamR teamId

deleteTeamR :: TeamId -> Handler Value
deleteTeamR teamId = do
    _ <- requireAuthPair
    runDB $ delete teamId
    redirect AdminR

deleteTeamMemberR :: TeamMemberId -> Handler Value
deleteTeamMemberR memberId = do
    (userId, user) <- requireAuthPair
    teamMember <- runDB $ get404 memberId
    let teamId = teamMemberTeamId teamMember
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
    _ <- requireAuthPair
    ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm addObjectiveForm

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

-- TEAM MEMBER FORM
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

-- TEAM OBJECTIVE FORM
data AddObjectiveForm = AddObjectiveForm
    { objectiveName' :: Text
    , objectiveDescription' :: Text
    }

addObjectiveForm :: AForm Handler AddObjectiveForm
addObjectiveForm = AddObjectiveForm
    <$> areq textField (bfs ("Name" :: Text)) Nothing
    <*> areq textField (bfs ("Description" :: Text)) Nothing
