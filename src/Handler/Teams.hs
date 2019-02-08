module Handler.Teams where

import Import

getTeamsR :: Handler Value
getTeamsR = do
    allComments <- runDB $ selectList [] [Asc TeamId]
    returnJson allComments

postTeamsR :: Handler Value
postTeamsR = do
    team <- (requireJsonBody :: Handler Team)
    inserted <- runDB $ insertEntity team
    returnJson inserted

getTeamR :: TeamId -> Handler Value
getTeamR teamId = do
    team <- runDB $ get404 teamId
    returnJson team

getTeamObjectivesR :: TeamId -> Handler Value
getTeamObjectivesR teamId = do
    objectives <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    returnJson (objectives :: [Entity Objective])

postTeamObjectivesR :: TeamId -> Handler Value
postTeamObjectivesR teamId = do
    objective <- (requireJsonBody :: Handler Objective)
    inserted <- runDB $ insertEntity objective
    returnJson inserted
