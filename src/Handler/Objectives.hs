module Handler.Objectives where

import Import

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

getResultsR :: ObjectiveId -> Handler Value
getResultsR objectiveId = do
    addHeader (pack "Access-Control-Allow-Origin") (pack "*")
    results <- runDB $ selectList [ResultObjectiveId ==. objectiveId] []
    returnJson results

postResultsR :: ObjectiveId -> Handler Value
postResultsR objId = do
    _ <- runDB $ get404 objId
    result <- (requireJsonBody :: Handler Result)
    inserted <- runDB $ insertEntity result
    returnJson inserted
