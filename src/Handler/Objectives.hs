{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Objectives where

import Database.Persist.Sql (fromSqlKey)
import Import
import Data.Maybe (fromMaybe)
import Yesod.Form.Bootstrap3
import qualified Data.List as L

getObjectivesR :: Handler Value
getObjectivesR = do
    allComments <- runDB $ selectList [] [Asc ObjectiveId]
    returnJson allComments

postObjectivesR :: Handler Value
postObjectivesR = do
    objective <- (requireJsonBody :: Handler Objective)
    inserted <- runDB $ insertEntity objective
    returnJson inserted

getObjectiveR :: ObjectiveId -> Handler Value
getObjectiveR objectiveId = do
    objective <- runDB $ get404 objectiveId
    returnJson objective

deleteObjectiveR :: ObjectiveId -> Handler Value
deleteObjectiveR objectiveId = do
    (userId, user) <- requireAuthPair
    objective <- runDB $ get404 objectiveId

    members <- runDB $ selectList [TeamMemberUserId ==. userId, TeamMemberTeamId ==. objectiveTeamId objective] []
    case members of
        [] -> redirect (TeamR $ objectiveTeamId objective)
        _ -> do
            runDB $ delete objectiveId
            redirect (TeamR $ objectiveTeamId objective)

getResultsR :: ObjectiveId -> Handler Value
getResultsR objectiveId = do
    addHeader (pack "Access-Control-Allow-Origin") (pack "*")
    results <- runDB $ selectList [ResultObjectiveId ==. objectiveId] []
    returnJson results

postResultsR :: ObjectiveId -> Handler Html
postResultsR objId = do
    (_, _) <- requireAuthPair
    ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm addResultForm
    objective <- runDB $ get404 objId

    case result of
        FormSuccess formInput -> do
            let ent = Result (resultName' formInput) (resultDescription' formInput) False objId
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    redirect $ TeamR $ objectiveTeamId objective

--OBJECTIVE RESULT FORM
data AddResultForm = AddResultForm
    { resultName' :: Text
    , resultDescription' :: Text
    }

addResultForm :: AForm Handler AddResultForm
addResultForm = AddResultForm
    <$> areq textField (bfs ("Name" :: Text)) Nothing
    <*> areq textField (bfs ("Description" :: Text)) Nothing
